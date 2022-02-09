open Ocaml_common
open Sess
open Parsetree

let bound_default = 20

type kmcspec =
  { protocol_name : string
  ; roles : string list
  ; kmc_bound : int
  }

type ppx_kmclib_state =
  { kmc_specs : (string, kmcspec) Hashtbl.t
  ; kmc_error_traces : (string, Runkmc.kmc_result) Hashtbl.t
  }

let typ_with_attr ~loc typ name payload =
  { typ with
    ptyp_attributes = [ Ast_helper.Attr.mk ~loc { txt = name; loc } payload ]
  }

class replace_holes =
  object
    inherit Ppxlib.Ast_traverse.map as super

    (* [%kmc.gen g (r1,r2,r3)] --> (assert false)[@kmc.gen g (r1,r2,r3)] *)
    method! expression exp0 =
      match exp0.pexp_desc with
      | Pexp_extension
          ({ txt = ("kmc.gen" | "kmc.gen.runner") as extname; loc }, payload) ->
        let any = Ast_helper.Typ.var (Util.fresh_var ()) in
        let exp = [%expr Kmclib.Internal.make_kmctup (assert false)] in
        let exp =
          { exp with
            pexp_attributes =
              [ { attr_name = { txt = extname; loc }
                ; attr_payload = payload
                ; attr_loc = loc
                }
              ]
          }
        in
        let exp = [%expr ([%e exp] : [%t any])] in
        exp
      | Pexp_constraint
          ( exp
          , { ptyp_desc =
                Ptyp_extension
                  ( { txt = ("kmc.check" | "kmc.check.handler") as extname; _ }
                  , payload )
            ; ptyp_loc
            ; _
            } ) ->
        let typ =
          typ_with_attr ~loc:ptyp_loc
            (Ast_helper.Typ.any ~loc:exp0.pexp_loc ())
            extname payload
        in
        { exp0 with pexp_desc = Pexp_constraint (super#expression exp, typ) }
      | _ -> super#expression exp0

    (* (pat : [%kmc.check gid.role]) --> (pat : _[@kmc.check gid.role]) *)
    method! pattern pat0 =
      match pat0.ppat_desc with
      | Ppat_constraint
          ( pat
          , { ptyp_desc =
                Ptyp_extension
                  ( { txt = ("kmc.check" | "kmc.check.handler") as extname; _ }
                  , payload )
            ; ptyp_loc
            ; _
            } ) ->
        let typ =
          typ_with_attr ~loc:ptyp_loc
            (Ast_helper.Typ.any ~loc:pat0.ppat_loc ())
            extname payload
        in
        let desc = Ppat_constraint (super#pattern pat, typ) in
        { pat0 with ppat_desc = desc }
      | _ -> super#pattern pat0
  end

(* XXX *)
let core_type_of_type_expr typ =
  Printtyp.reset_and_mark_loops typ;
  let otyp = Printtyp.tree_of_typexp false typ in
  let typ_str = Format.asprintf "%a" !Oprint.out_type otyp in
  Parse.core_type (Lexing.from_string typ_str)

let ppwarning ~loc str =
  let payload : expression =
    Ast_helper.Exp.constant (Ast_helper.Const.string str)
  in
  { attr_name = { txt = "ppwarning"; loc = Location.none }
  ; attr_payload =
      PStr [ { pstr_desc = Pstr_eval (payload, []); pstr_loc = loc } ]
  ; attr_loc = Location.none
  }

let mark_alert_exp loc exp str : expression =
  { exp with pexp_attributes = exp.pexp_attributes @ [ ppwarning ~loc str ] }

let mark_alert_typ loc typ str : core_type =
  { typ with ptyp_attributes = typ.ptyp_attributes @ [ ppwarning ~loc str ] }

let ext_error ~loc msg =
  ( { Location.txt = "ocaml.error"; loc }
  , PStr
      [ { pstr_desc =
            Pstr_eval
              (Ast_helper.Exp.constant ~loc (Ast_helper.Const.string msg), [])
        ; pstr_loc = loc
        }
      ] )

let exp_error ~loc msg = Ast_helper.Exp.extension ~loc @@ ext_error ~loc msg

let typ_error ~loc msg = Ast_helper.Typ.extension ~loc @@ ext_error ~loc msg

let exp_error_typed ~loc ~wrapped msg types =
  let typ = Ast_helper.Typ.tuple ~loc (List.map (fun (_, typ) -> typ) types) in
  let typ =
    if wrapped then
      [%type: [%t typ] kmctup]
    else
      typ
  in
  Ast_helper.Exp.constraint_ ~loc (exp_error ~loc msg) typ

let kmcspec_of_payload ~loc exp : kmcspec =
  let list_of_exps exps =
    List.map
      (function
        | { pexp_desc = Pexp_ident id; _ } -> Util.string_of_longident id.txt
        | e ->
          Location.raise_errorf ~loc:e.pexp_loc "Bad role name: %a"
            Pprintast.expression e)
      exps
  in
  let protocol_name = ref ""
  and roles = ref None
  and kmc_bound = ref bound_default in
  let rec parse = function
    | (Asttypes.Nolabel, { pexp_desc = Pexp_ident id; _ }) :: xs ->
      protocol_name := Util.string_of_longident id.txt;
      parse xs
    | (Nolabel, { pexp_desc = Pexp_tuple exps; _ }) :: xs ->
      roles := Some (list_of_exps exps);
      parse xs
    | ( Labelled "bound"
      , { pexp_desc = Pexp_constant (Pconst_integer (intstr, None)); _ } )
      :: xs ->
      kmc_bound := int_of_string intstr;
      parse xs
    | [] -> ()
    | (_, exp) :: _ ->
      Location.raise_errorf ~loc:exp.pexp_loc "Malformed kmc specification: %a"
        Pprintast.expression exp
  in
  match exp.pexp_desc with
  | Pexp_tuple exps ->
    { protocol_name = ""; roles = list_of_exps exps; kmc_bound = bound_default }
  | Pexp_apply (exp, exps) -> (
    parse ((Nolabel, exp) :: exps);
    match !roles with
    | Some roles ->
      { protocol_name = !protocol_name; roles; kmc_bound = !kmc_bound }
    | None ->
      Location.raise_errorf ~loc:exp.pexp_loc "Role not given: %a"
        Pprintast.expression exp)
  | _ ->
    Location.raise_errorf ~loc "Must be a tuple of roles: %a"
      Pprintast.expression exp

let fieldref_of_payload ~loc exp =
  match exp.pexp_desc with
  | Pexp_field
      ({ pexp_desc = Pexp_ident { txt = sysname; _ }; _ }, { txt = rolename; _ })
    ->
    (Util.string_of_longident sysname, Util.string_of_longident rolename)
  | Pexp_ident id -> ("", Util.string_of_longident id.txt)
  | _ ->
    (* warning? *)
    Location.raise_errorf ~loc "bad projection specification: %a"
      Pprintast.expression exp

let make_checked_type ~loc attr_name typ orig_attrs =
  let inferred = core_type_of_type_expr typ in
  let translated =
    if attr_name = "kmc.check" then
      Chvectyp.to_session_type inferred
    else
      Handlertyp.handler_type_to_session_type inferred
  in
  let typ =
    match translated with
    | Left st ->
      (* no errors, put the inferred type and annotate it with session types *)
      let any = Ast_helper.Typ.any ~loc () in
      mark_alert_typ loc any (show_sess st)
    | Right errtyp ->
      (* type format errors -- put types decorated with errors *)
      let msg =
        Format.asprintf "inferred: %a\nrewritten: %a" Pprintast.core_type
          inferred Pprintast.core_type errtyp
      in
      mark_alert_typ loc errtyp msg
  in
  { typ with ptyp_loc = loc; ptyp_attributes = orig_attrs }

(* - translates hole[@kmc.gen g (a,b,c)] --> <<tuple of channel vectors for
   a,b,c in g >> - Invokes KMC checker and record error traces in the state *)
let transl_kmc_gen (state : ppx_kmclib_state) (super : Untypeast.mapper)
    (self : Untypeast.mapper) (exp : Typedtree.expression) =
  match exp.exp_extra with
  | ( Texp_constraint
        { ctyp_attributes =
            { attr_name = { txt = attr_name; _ }; attr_loc = loc; _ } :: _ as
            attrs
        ; _
        }
    , extra_loc
    , _ )
    :: rem ->
    let typ = make_checked_type ~loc attr_name exp.exp_type attrs in
    let exp = self.expr self { exp with exp_extra = rem } in
    Ast_helper.Exp.constraint_ ~loc:extra_loc exp typ
  | _ -> (
    match exp.exp_attributes with
    | [ { attr_name = { txt = ("kmc.gen" | "kmc.gen.runner") as extname; _ }
        ; attr_payload = payload
        ; attr_loc = loc
        }
      ] -> (
      match payload with
      | PStr [ { pstr_desc = Pstr_eval (spec, _); _ } ] -> (
        let ptyp = core_type_of_type_expr exp.exp_type in
        let spec = kmcspec_of_payload ~loc spec in
        Hashtbl.add state.kmc_specs spec.protocol_name spec;
        let wrapped, sts =
          if extname = "kmc.gen" then
            Chvectyp.to_session_types ~loc spec.roles ptyp
          else
            Handlertyp.runner_type_to_session_types ~loc spec.roles ptyp
        in
        match sts with
        | Right typs ->
          (* type format errors -- generate holes with erroneous types *)
          exp_error_typed ~loc ~wrapped "Format Error (see types)" typs
        | Left sts -> (
          (* session types successfully inferred -- now check them with KMC
             checker *)
          match Runkmc.run ~hi:spec.kmc_bound sts with
          | () ->
            (* Safe! *)
            let msg =
              "\nsession types: "
              ^ String.concat "; "
                  (List.map
                     (fun (role, st) -> showrole role ^ ": " ^ show_sess st)
                     sts)
            in
            let exp =
              if extname = "kmc.gen" then
                Chvecexp.make_chvecs ~loc sts
              else
                Handlerexp.make_handlers ~loc sts
            in
            let _msg =
              msg ^ "\n" ^ Format.asprintf "%a\n" Pprintast.expression exp
            in
            let exp =
              if wrapped then
                [%expr Kmclib.Internal.make_kmctup [%e exp]]
              else
                exp
            in
            exp
            (* mark_alert_exp loc exp msg *)
          | exception Runkmc.KMCFail msg ->
            Location.raise_errorf ~loc "%s" ("KMC checker failed:" ^ msg)
          | exception Runkmc.KMCUnsafe result ->
            Hashtbl.add state.kmc_error_traces spec.protocol_name result;
            exp_error ~loc @@ "KMC system unsafe:\n"
            ^ String.concat "\n" result.lines
            ^ "\nInput:" ^ result.input))
      | PStr p ->
        Location.raise_errorf ~loc "Bad payload: %a" Pprintast.structure p
      | _ -> Location.raise_errorf ~loc "Payload format not applicable")
    | _ -> super.expr self exp)

(* (pat : _[@kmc.check g.r]) --> (pat : <<channel vector type of r in
   g>>[@kmc.check g.r]) *)
let transl_kmc_check_form (super : Untypeast.mapper) (self : Untypeast.mapper)
    pat =
  let open Typedtree in
  match pat.pat_extra with
  | ( Tpat_constraint
        { ctyp_attributes =
            [ { attr_name =
                  { txt = ("kmc.check" | "kmc.check.handler") as attr_name; _ }
              ; attr_loc = loc
              ; _
              }
            ] as attr
        ; _
        }
    , extra_loc
    , _ )
    :: rem ->
    let typ = make_checked_type ~loc attr_name pat.pat_type attr in
    let desc =
      Ppat_constraint (super.pat self { pat with pat_extra = rem }, typ)
    in
    Ast_helper.Pat.mk ~loc:extra_loc desc
  | _ -> super.pat self pat

let make_untyper state =
  let super = Untypeast.default_mapper in
  { super with
    expr = (fun self -> transl_kmc_gen state super self)
  ; pat = (fun self -> transl_kmc_check_form super self)
  }

let project_trace ~msg role (actions : Runkmc.action list) =
  List.fold_left
    (fun sess Runkmc.{ from; to_; mode; label; payload } ->
      let from = Sess.readrole from
      and to_ = Sess.readrole to_ in
      match mode with
      | Out when from = role -> Sess.Out (to_, [ (label, (payload, sess)) ])
      | Inp when to_ = role -> Sess.Inp (from, [ (label, (payload, sess)) ])
      | _ -> sess)
    (Err msg) (List.rev actions)

let generate_trace_types roles (res : Runkmc.kmc_result) =
  let make_traces msg trace =
    List.map (fun role -> (role, project_trace ~msg role trace)) roles
  in
  let progress =
    if res.progress_violation = [] then
      []
    else
      let trace = List.hd (List.rev res.progress_violation) in
      [ ("progress_violation", List.length trace, trace) ]
  in
  let eventual =
    List.map
      (fun trace -> ("eventual_reception_violation", List.length trace, trace))
      res.eventual_reception_violation
  in
  let sorted =
    List.sort
      (fun (_, len1, _) (_, len2, _) -> len1 - len2)
      (progress @ eventual)
  in
  if sorted <> [] then
    let kind, _, trace = List.hd sorted in
    make_traces kind trace
  else
    []

(* insert error traces using the state *)
class insert_kmc_error_traces_as_types (state : ppx_kmclib_state) =
  object
    inherit Ppxlib.Ast_traverse.map as super

    method! core_type typ =
      match typ.ptyp_attributes with
      | { attr_name =
            { txt = ("kmc.check" | "kmc.check.handler") as attrname; _ }
        ; attr_payload = PStr [ { pstr_desc = Pstr_eval (payload, _); _ } ]
        ; attr_loc
        ; _
        }
        :: attr_rem -> (
        let g, role = fieldref_of_payload ~loc:typ.ptyp_loc payload in
        match Hashtbl.find_opt state.kmc_specs g with
        | None when g = "" ->
          typ_error ~loc:typ.ptyp_loc ("No such protocol : " ^ g)
        | None ->
          typ_error ~loc:typ.ptyp_loc
            "No protocol found. Did you forget the protocol name (like g.x)?"
        | Some spec -> (
          if not @@ List.mem role spec.roles then
            typ_error ~loc:typ.ptyp_loc ("No such role: " ^ role)
          else
            try
              let result = Hashtbl.find state.kmc_error_traces g in
              let traces = generate_trace_types [ role ] result in
              let st = List.assoc role traces in
              let typ =
                if attrname = "kmc.check" then
                  Chvectyp.make_chvec_type ~loc:attr_loc st
                else
                  Handlertyp.make_handler_type ~loc:attr_loc st
              in
              { typ with ptyp_attributes = attr_rem }
            with
            | Not_found -> super#core_type typ))
      | _ -> super#core_type typ
  end

let transform str =
  let str = (new replace_holes)#structure str in
  let env =
    Compmisc.init_path ();
    Compmisc.initial_env ()
  in
  let tstr, _, _, _ = Typemod.type_structure env str in
  let state =
    { kmc_specs = Hashtbl.create 42; kmc_error_traces = Hashtbl.create 42 }
  in
  let untyper = make_untyper state in
  let str = untyper.structure untyper tstr in
  (new insert_kmc_error_traces_as_types state)#structure str

let () = Ppxlib.Driver.register_transformation ~impl:transform "ppx_kmclib"
