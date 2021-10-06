open Ocaml_common
open Sess
open Parsetree

type ppx_jrklib_state = {kmc_error_traces: (string, Runkmc.kmc_result) Hashtbl.t}

let typ_with_attr ~loc typ name payload =
  {typ with ptyp_attributes=[Ast_helper.Attr.mk ~loc {txt=name;loc} payload]}

class replace_holes = object
  inherit Ppxlib.Ast_traverse.map as super

  (* [%kmc.gen g (r1,r2,r3)] --> (assert false)[@kmc.gen g (r1,r2,r3)] *)
  method! expression exp = 
    match exp.pexp_desc with
    | Pexp_extension({txt="kmc.gen"|"kmc.gen.runner" as extname;loc},payload) ->
      let any = Ast_helper.Typ.var (Util.fresh_var ()) in
      let exp = [%expr assert false] in
      let exp = {exp with pexp_attributes=[{attr_name={txt=extname;loc}; attr_payload=payload; attr_loc=loc}]} in
      let exp = [%expr ([%e exp] : [%t any])] in
      exp
    | _ ->
      super#expression exp

  (* (pat : [%kmc.check gid.role]) --> (pat : _[@kmc.check gid.role])  *)
  method! pattern pat0 =
    match pat0.ppat_desc with
    | Ppat_constraint (pat, {ptyp_desc=Ptyp_extension({txt="kmc.check"; _}, payload); ptyp_loc; _}) ->
      let typ =
        typ_with_attr 
          ~loc:ptyp_loc 
          (Ast_helper.Typ.any ~loc:pat0.ppat_loc ())
          "kmc.check" 
          payload
      in
      let desc = Ppat_constraint(super#pattern pat, typ) in
      {pat0 with ppat_desc=desc}
    | _ -> 
      pat0
end

(* XXX *)
let core_type_of_type_expr typ =
  Printtyp.reset_and_mark_loops typ;
  let otyp = Printtyp.tree_of_typexp false typ in
  let typ_str = Format.asprintf "%a" !Oprint.out_type otyp in
  Parse.core_type (Lexing.from_string typ_str)

let make_tuple_hole ~loc types =
  let typ = Ast_helper.Typ.tuple ~loc (List.map (fun (_,typ) -> typ) types) in
  Ast_helper.Exp.constraint_ ~loc [%expr assert false] typ

let ppwarning ~loc str =
  let payload : expression = 
    Ast_helper.Exp.constant (Ast_helper.Const.string str)
  in
  {
    attr_name={txt="ppwarning";loc=Location.none}; 
    attr_payload=PStr[{pstr_desc=Pstr_eval(payload,[]); pstr_loc=loc}]; 
    attr_loc=Location.none
  }

let mark_alert_exp loc exp str : expression =
  {exp with pexp_attributes=exp.pexp_attributes @ [ppwarning ~loc str]}

let mark_alert_typ loc typ str : core_type =
  {typ with ptyp_attributes=typ.ptyp_attributes @ [ppwarning ~loc str]}

let exp_error ~loc msg =
  Ast_helper.Exp.extension ~loc 
    ({Location.txt="ocaml.error";loc}, 
    PStr [{pstr_desc=Pstr_eval(Ast_helper.Exp.constant ~loc (Ast_helper.Const.string msg),[]); 
           pstr_loc=loc}])

type rolespec = string option * string list

let rolespec_of_payload ~loc exp : rolespec =
  let list_of_exps exps =
    List.map 
    (function 
      | {pexp_desc=Pexp_ident(id); _} -> Util.string_of_longident id.txt 
      | e -> Location.raise_errorf ~loc:e.pexp_loc "Bad role name: %a" Pprintast.expression e) 
    exps
  in
  match exp.pexp_desc with
  | Pexp_tuple(exps) -> 
    None, list_of_exps exps
  | Pexp_apply({pexp_desc=Pexp_ident(id); _}, [(_, {pexp_desc=Pexp_tuple(exps); _})]) -> 
    Some (Util.string_of_longident id.txt), list_of_exps exps
  | _ ->
    Location.raise_errorf ~loc "Must be a tuple of roles: %a" Pprintast.expression exp

(* 
 - translates
   hole[@kmc.gen g (a,b,c)] 
   -->
   <<tuple of channel vectors for a,b,c in g >>
 - Invokes KMC checker and record error traces in the state
 *)
let transl_kmc_gen
  (state : ppx_jrklib_state)
  (super : Untypeast.mapper) 
  (self : Untypeast.mapper) 
  (exp : Typedtree.expression) =
  match exp.exp_attributes with
  | [{attr_name={txt=("kmc.gen"|"kmc.gen.runner" as extname); _};attr_payload=payload; attr_loc=loc}] ->
    begin match payload with 
    | PStr[{pstr_desc=Pstr_eval(rolespec,_);_}] -> 
      let ptyp = core_type_of_type_expr exp.exp_type in
      let sysname, rolespec = rolespec_of_payload ~loc rolespec in
      let sts = 
        if extname = "kmc.gen" then
          Chvectyp.to_session_types ~loc rolespec ptyp 
        else
          Handlertyp.to_session_types ~loc rolespec ptyp 
      in
      begin match sts with
      | Right typs -> 
        (* type format errors -- generate holes with erroneous types *)
        make_tuple_hole ~loc typs
      | Left sts -> 
        (* session types successfully inferred -- now check them with KMC checker *)
        begin match Runkmc.run sts with
        | () -> 
          (* Safe! *)
          let msg = 
            "\nsession types: " ^ 
            String.concat "; " (List.map (fun (role,st) -> showrole role ^ ": " ^ show_sess st) sts)
          in
          let exp = 
            if extname = "kmc.gen" then
              Chvecexp.make_chvecs ~loc sts
            else
              Handlerexp.make_handlers ~loc sts
          in
          let msg = msg ^ "\n" ^
            Format.asprintf "%a\n" Pprintast.expression exp
          in
          mark_alert_exp loc exp msg
        | exception Runkmc.KMCFail(msg) ->
          Location.raise_errorf ~loc "%s" ("KMC checker failed:"^msg)
        | exception Runkmc.KMCUnsafe(result) ->
          sysname |> Option.iter (fun name -> Hashtbl.add state.kmc_error_traces name result);
          exp_error ~loc
            @@ "KMC system unsafe:\n" ^ String.concat "\n" result.lines ^ "\nInput:" ^ result.input
        end
      end
    | PStr p -> 
      Location.raise_errorf ~loc "Bad payload: %a" Pprintast.structure p
    | _ -> 
      Location.raise_errorf ~loc "Payload format not applicable"
    end
  | _ ->
    super.expr self exp

(* (pat : _[@kmc.check g.r]) --> (pat : <<channel vector type of r in g>>[@kmc.check g.r]) *)
let transl_kmc_check (super : Untypeast.mapper) (self : Untypeast.mapper) pat =
  let open Typedtree in
  match pat.pat_extra with
  | (Tpat_constraint({ctyp_attributes=([ {attr_name={txt="kmc.check"; _}; attr_loc=loc; _} ] as attrs); _}), extra_loc, _) :: rem ->
    let inferred = core_type_of_type_expr pat.pat_type in
    let typ = 
      let typ, msg =
        match Chvectyp.to_session_type inferred with
        | Left st ->
          (* no errors, put the inferred type and annotate it with session types *)
          inferred, show_sess st
        | Right errtyp -> 
          (* type format errors -- put types decorated with errors *)
          errtyp, Format.asprintf "inferred: %a\nrewritten: %a" Pprintast.core_type inferred Pprintast.core_type errtyp      
        in
        mark_alert_typ loc {typ with ptyp_loc=loc; ptyp_attributes=attrs} msg
    in
    let desc = Ppat_constraint(super.pat self {pat with pat_extra=rem}, typ) in
    Ast_helper.Pat.mk ~loc:extra_loc desc
  | _ ->
    super.pat self pat

let make_untyper state = 
  let super = Untypeast.default_mapper in
  {super with 
    expr = (fun self -> transl_kmc_gen state super self);
    pat = (fun self -> transl_kmc_check super self)
  }

let fieldref_of_payload ~loc exp =
  match exp.pexp_desc with
  | Pexp_field({pexp_desc=Pexp_ident({txt=sysname; _}); _}, {txt=rolename; _}) ->
    Util.string_of_longident sysname, Util.string_of_longident rolename
  | _ ->
    (* warning? *)
    Location.raise_errorf ~loc "bad projection specification: %a" Pprintast.expression exp

let project_trace ~msg role (actions:Runkmc.action list) =
  List.fold_left (fun sess Runkmc.{from;to_;mode;label;payload} ->
      let from = Sess.readrole from
      and to_ = Sess.readrole to_ in
      match mode with
      | Out when from=role -> Sess.Out(to_,[(label,(payload,sess))])
      | Inp when to_=role -> Sess.Inp(from,[(label,(payload,sess))])
      | _ -> sess
    ) (Err msg) (List.rev actions)

let generate_trace_types roles (res:Runkmc.kmc_result) =
  let make_traces msg trace =
    List.map (fun role -> role, project_trace ~msg role trace) roles
  in
  if res.progress_violation <> [] then 
    make_traces "progress_violation" (List.hd res.progress_violation) (* XXX FIXME *)
  else if res.eventual_reception_violation <> [] then
    make_traces "eventual_reception_violation" (List.hd res.eventual_reception_violation) (* XXX FIXME *)
  else
    []

(* insert error traces using the state *)
class insert_kmc_error_traces_as_types (state:ppx_jrklib_state) = object
  inherit Ppxlib.Ast_traverse.map as super

  method! core_type typ =
    match typ.ptyp_attributes with
    | {attr_name={txt="kmc.check"; _}; attr_payload=PStr[{pstr_desc=Pstr_eval(payload,_); _}]; attr_loc; _} :: attr_rem ->
      let g, role = fieldref_of_payload ~loc:typ.ptyp_loc payload in
      begin match Hashtbl.find_opt state.kmc_error_traces g with
      | Some result ->
        let traces = generate_trace_types [role] result in
        begin match List.assoc_opt role traces with
        | Some st -> 
          let typ = Chvectyp.make_chvec_type ~loc:attr_loc st in
          {typ with ptyp_attributes=attr_rem}
        | None -> 
          super#core_type typ
        end
      | None ->
       super#core_type typ
      end
    | _ ->
      super#core_type typ
end

let transform str =
  let str = (new replace_holes)#structure str in
  let env = 
    Compmisc.init_path (); 
    Compmisc.initial_env () 
  in
  let (tstr, _, _, _) = 
    Typemod.type_structure env str 
  in
  let state = 
    {kmc_error_traces=Hashtbl.create 42} 
  in
  let untyper = make_untyper state in
  let str = untyper.structure untyper tstr in
  (new insert_kmc_error_traces_as_types state)#structure str


let () = Ppxlib.Driver.register_transformation
  ~impl: transform
  "ppx_jrklib"
