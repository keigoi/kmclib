open Ocaml_common
open Sess
open Parsetree

type key = RolePair of string * string | Label of string
type tbl = (key, string * expression) Hashtbl.t

let get_or_make (tbl:tbl) key f =
  match Hashtbl.find_opt tbl key with
  | Some v -> v
  | None ->
    let v = f () in
    Hashtbl.add tbl key v;
    v

let fresh_var =
  let cnt = ref 0 in
  fun ?(prefix="kmclib_var_") ?(suffix="") () ->
  let s = prefix ^ string_of_int !cnt ^ suffix in
  cnt := !cnt + 1;
  s


let pat_attr ~loc pat name payload =
  {pat with ppat_attributes=[Ast_helper.Attr.mk ~loc {txt=name;loc} payload]}

let typ_attr ~loc typ name payload =
  {typ with ptyp_attributes=[Ast_helper.Attr.mk ~loc {txt=name;loc} payload]}

class replace_holes = object
  inherit Ppxlib.Ast_traverse.map as super

  method! expression exp = 
    match exp.pexp_desc with
    | Pexp_extension({txt="kmc.gen";loc},payload) ->
      let any = Ast_helper.Typ.var (fresh_var ()) in
      let exp = [%expr assert false] in
      let exp = {exp with pexp_attributes=[{attr_name={txt="kmc.gen";loc}; attr_payload=payload; attr_loc=loc}]} in
      let exp = [%expr ([%e exp] : [%t any])] in
      exp
    | _ ->
      super#expression exp

  method! pattern pat0 =
    match pat0.ppat_desc with
    | Ppat_constraint (pat, {ptyp_desc=Ptyp_extension({txt="kmc.check"; _}, payload); ptyp_loc; _}) ->
      let typ =
        typ_attr 
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

let rec string_of_out_ident : Outcometree.out_ident -> string = function
  | Oide_apply(_,id) ->  string_of_out_ident id
  | Oide_dot(_,id) -> id
  | Oide_ident {printed_name} -> printed_name

let rec string_of_longident : Longident.t -> string = function
  | Lapply(_,id) -> string_of_longident id
  | Ldot(_,id) -> id
  | Lident(id) -> id

let string_of_ptyp = Format.asprintf "%a" Pprintast.core_type

let var_pat ?(loc=Location.none) varname =
  Ast_helper.Pat.var ~loc {txt=varname;loc}

let var_exp ?(loc=Location.none) varname =
  Ast_helper.Exp.ident ~loc {txt=Longident.Lident varname;loc}

let make_label ?(loc=Location.none) : string -> string * expression = fun label ->
  let pat constr = Ast_helper.Pat.variant constr (Some (var_pat "x")) in
  let exp constr = Ast_helper.Exp.variant constr (Some (var_exp "x")) in
  let var = fresh_var ~prefix:("lab_"^label) () in
  var, [%expr {match_var=(function [%p pat label] -> Some x | _ -> None); make_var=(fun x -> [%e exp label])}]

let method_ ?(loc=Location.none) name exp =
  Ast_helper.Cf.method_ ~loc
    (Location.mkloc name loc) 
    Public 
      (Cfk_concrete(Fresh, exp))

let tycon ?(loc=Location.none) name =
  Ast_helper.Typ.constr {txt=(Longident.Lident name);loc} []

let make_out ?(loc=Location.none) ~(tbl:tbl) ch label payload cont =
  let constr, _ = get_or_make tbl (Label label) (fun () -> make_label ~loc label) in
  [%expr (Internal.make_out_lazy [%e ch] [%e var_exp constr] [%e cont] : ([%t tycon ~loc payload],_) out)]

let make_inp ?(loc=Location.none) ~(tbl:tbl) ch label payload cont =
  let constr, _ = get_or_make tbl (Label label) (fun () -> make_label ~loc label) in
  let payload = tycon ~loc payload in
  let polyvar = Ast_helper.Typ.variant [Ast_helper.Rf.tag {txt=label;loc} false [[%type: [%t payload] * _ ]]] Asttypes.Open None in
  [%expr (Internal.make_inp_lazy [%e ch] [%e var_exp constr] [%e cont] : [%t polyvar] inp) ]

let make_object ?(loc=Location.none) methods =
  Ast_helper.Exp.object_ @@ Ast_helper.Cstr.mk [%pat? _] methods

let meta_fold_left_ ?(loc=Location.none) f_meta exps =
  List.fold_left (fun e1 e2 -> [%expr [%e f_meta] [%e e1] [%e e2]]) (List.hd exps) (List.tl exps)

let let_ ?(loc=Location.none) bindings exp =
  List.fold_left (fun body (var,exp) -> 
      [%expr let [%p var_pat var] = [%e exp] in [%e body]]) 
    exp 
    bindings

let let_tbl ?(loc=Location.none) ?(check=(fun _ -> ())) tbl exp =
  Hashtbl.fold
  (fun key (var,exp) body ->
    check key;
    [%expr let [%p var_pat var] = [%e exp] in [%e body]]
  )
  tbl exp

let letrec ?(loc=Location.none) bindings exp =
  Ast_helper.Exp.let_ ~loc
    Asttypes.Recursive
    (List.map (fun (var,exp) -> Ast_helper.Vb.mk ~loc (var_pat ~loc var) exp) bindings)
    exp

let lazy_ ?(loc=Location.none) exp = 
  [%expr lazy [%e exp]]

let lazy_val ?(loc=Location.none) exp = 
  [%expr Lazy.from_val [%e exp]]

let make_channel ?(loc=Location.none) (r1,r2) () =
  let var = fresh_var ~prefix:("ch") ~suffix:("_"^r1^"_"^r2) () in
  (var, [%expr Domainslib.Chan.make_unbounded ()])

let make_chvec ?(loc=Location.none) (tbl:tbl) self = 
  let rec loop = function
    | Out(role,conts) ->
      let ch, _ = get_or_make tbl (RolePair (self,role)) (make_channel (self,role)) in
      let outs = 
        List.map (fun (label, (payload, cont)) -> 
            let cont = loop cont in
            (label, make_out ~loc ~tbl (var_exp ch) label payload cont)
          ) conts
      in
      lazy_val @@
      let_ ~loc outs @@
        make_object ~loc @@
          [method_ ~loc role @@ make_object ~loc @@
            List.map (fun (label, _) -> method_ ~loc label (var_exp label)) outs]
    | Inp(role,conts) ->
      let ch, _ = get_or_make tbl (RolePair (role,self)) (make_channel (role,self)) in
      let inps =
        meta_fold_left_ ~loc [%expr Internal.merge_inp] @@ 
            List.map (fun (label, (payload,cont)) -> 
              let cont = loop cont in
              make_inp ~loc ~tbl (var_exp ch) label payload cont) conts
      in
      let var = fresh_var() in
      lazy_val  @@
      let_ ~loc [(var,inps)] @@
        (make_object ~loc [method_ ~loc role (var_exp var)])
    | End ->
      [%expr Lazy.from_val ()]
    | Rec(var,st) ->
      let exp = loop st in
      letrec ~loc
        [(var, lazy_ [%expr Lazy.force_val [%e exp]])] 
        [%expr ignore (Lazy.force [%e var_exp var]); [%e var_exp var]]
    | Var var ->
      var_exp var
    | Err msg ->
      Location.raise_errorf ~loc "Impossible: Trying to generate an erroneous channel vector %s" msg
  in
  fun st ->
    let exp = loop st in
    [%expr Lazy.force_val [%e exp]]

let make_chvecs ~loc sts =
  let tbl = Hashtbl.create 42 in
  let exps = List.map (fun (role,st) -> make_chvec tbl role st) sts in
  let exp = Ast_helper.Exp.tuple exps ~loc in
  let exp = let_tbl ~loc tbl exp in
  exp

let chvec_errtyp ?(loc=Location.none) msg =
  Ast_helper.Typ.variant ~loc [Ast_helper.Rf.tag {txt=msg;loc=Location.none} false []] Asttypes.Closed None

let make_chvec_type ~loc =
  let out cont =
    Ast_helper.Typ.constr ~loc 
      {txt=Longident.Lident "out"; loc} 
      [Ast_helper.Typ.any ~loc (); cont]
  in
  let rec loop = function
    | Out(role,conts) ->
      let flds =
        List.map (fun (lab, (_typ, cont)) -> 
            Ast_helper.Of.tag 
              ~loc 
              {txt=lab;loc} 
              (out (loop cont))
          ) conts
      in
      Ast_helper.Typ.object_ ~loc [
        Ast_helper.Of.tag ~loc 
          {txt=role;loc}
          (Ast_helper.Typ.object_ ~loc flds Asttypes.Open)
      ] Asttypes.Open
    | Inp(role,conts) ->
      let inp conts =
        let flds =
          List.map
            (fun (lab,(_typ,cont)) ->
              Ast_helper.Rf.tag ~loc
                {txt=lab;loc}
                false
                [Ast_helper.Typ.tuple ~loc [Ast_helper.Typ.any ~loc (); loop cont]]
              ) conts
        in
        Ast_helper.Typ.constr ~loc
          {txt=Longident.Lident "inp"; loc}
          [Ast_helper.Typ.variant ~loc
            flds
            Asttypes.Open
            None]
      in
      Ast_helper.Typ.object_ ~loc [
        Ast_helper.Of.tag ~loc 
          {txt=role;loc}
          (inp conts)
      ] Asttypes.Open
    | End ->
      Ast_helper.Typ.constr ~loc {txt=Longident.Lident "unit";loc} []
    | Rec(var,st) ->
      Ast_helper.Typ.alias ~loc (loop st) var
    | Var var ->
      Ast_helper.Typ.var ~loc var
    | Err msg ->
      chvec_errtyp msg
  in
  loop 


type 'x exit = {f:'t. 'x -> 't}

let map_all (exit:_ exit) f =
  let rec loop (acc,visited) = function
    | [] -> List.rev acc
    | x::xs ->
      let exit = {f=fun x -> exit.f (List.rev visited @ x::xs)} in
      begin match f exit x with
      | Some y -> loop (y::acc,x::visited) xs
      | None -> loop (acc,x::visited) xs
      end
  in
  loop ([],[])

let rec to_session_type vars =
  (* 'v * 'cont (in the input types of form [`lab of 'v * 'cont] inp) *)
  let input_pair exit ty = match ty.ptyp_desc with
    | Ptyp_tuple([pld;cont]) ->
      let exit =
        {f=fun cont -> exit.f {ty with ptyp_desc = Ptyp_tuple([pld;cont])}} 
      in
      string_of_ptyp pld, to_session_type vars exit cont
    | _ -> exit.f (chvec_errtyp "should_be_a_payload_cont_pair")
  in
  (* [`lab of _] *)
  let rtag exit r = match r.prf_desc with
    | Rtag(lab,opt,typs) ->
      if List.length typs = 1 then 
        let exit = 
          {f=fun typ -> exit.f {r with prf_desc=Rtag(lab,opt,[typ])}} 
        in
        Some (lab.txt, input_pair exit (List.hd typs))
      else
        exit.f {r with prf_desc=Rtag(lab,opt,[chvec_errtyp "should_not_be_a_conjunction"])}
    | _ -> 
      None
  in
  (* _ inp *)
  let input_variant exit typ =
    match typ.ptyp_desc with
    | Ptyp_variant (flds,x,y) ->
      let exit = 
        {f=fun flds-> exit.f {typ with ptyp_desc=Ptyp_variant(flds,x,y)}} 
      in
      map_all exit rtag flds
    | _ -> 
      exit.f (chvec_errtyp "should_be_a_polymorphic_variant_type")
  in
  (* ('pld * 'cont) out *)
  let output exit typ =
    match typ.ptyp_desc with
    | Ptyp_constr(name,[pld;cont]) when string_of_longident name.txt = "out" ->
      let exit = 
        {f=fun cont -> exit.f {typ with ptyp_desc=Ptyp_constr(name,[pld;cont])}} 
      in
      string_of_ptyp pld, to_session_type vars exit cont
    | _ ->
      exit.f (chvec_errtyp "should_be_an_output_type")
  in
  (* <lab: typ> *)
  let otag exit o =
    match o.pof_desc with
    | Otag(lab, typ) -> 
      let exit = 
        {f=fun typ -> exit.f {o with pof_desc=Otag(lab, typ)}} 
      in
      Some (lab.txt, output exit typ)
    | _ -> None
  in
  (* <lab: typ> or [`lab of typ] inp *)
  let method_ exit role typ =
    match typ.ptyp_desc with
    | Ptyp_object (flds, x) ->
      let exit = 
        {f=fun flds-> exit.f {typ with ptyp_desc=Ptyp_object(flds,x)}} 
      in
      Out(role.Location.txt, 
          map_all exit otag flds)
    | Ptyp_constr (name, [variant]) when string_of_longident name.txt = "inp" ->
      let exit = 
        {f=fun variant-> exit.f {typ with ptyp_desc=Ptyp_constr(name, [variant])}} 
      in
      Inp(role.txt, 
          input_variant exit variant)
    | _ -> 
      exit.f (chvec_errtyp "should_be_inp_or_output_object")
  in
  fun exit (ty:core_type) -> match ty.ptyp_desc with
  (* <role: typ> -- input or output *)
  | Ptyp_object ([{pof_desc=Otag(role, typ); _} as pof], flag) ->
    let exit = 
      {f=fun typ -> exit.f {ty with ptyp_desc=Ptyp_object([{pof with pof_desc=Otag(role, typ)}], flag)}}
    in
    method_ exit role typ
  (* unit *)
  | Ptyp_constr (name, [])
      when string_of_longident name.txt = "unit" ->
    End
  (* recursion variable *)
  | Ptyp_var var ->
    if List.mem var vars then
      Var var
    else
      exit.f (chvec_errtyp @@ "unbound_type_variable_" ^ var)
  (* recursion *)
  | Ptyp_alias (t,var) ->
    let exit =
      {f=fun typ -> exit.f {ty with ptyp_desc=Ptyp_alias(typ,var)}}
    in
    Rec(var, to_session_type (var::vars) exit t)
  (* error *)
  | _ -> 
    exit.f (chvec_errtyp "should_be_a_role_object_or_a_unit_type")

exception FormatError of core_type

let to_session_type ty = 
  let exit =
    {f=fun ty -> raise (FormatError ty)}
  in
  try
    Either.Left (to_session_type [] exit ty)
  with
    FormatError(ty) ->
      Either.Right ty  

type rolespec = string option * string list

let rolespec_of_payload ~loc exp : rolespec =
  let list_of_exps exps =
    List.map 
    (function 
      | {pexp_desc=Pexp_ident(id); _} -> string_of_longident id.txt 
      | e -> Location.raise_errorf ~loc:e.pexp_loc "Bad role name: %a" Pprintast.expression e) 
    exps
  in
  match exp.pexp_desc with
  | Pexp_tuple(exps) -> 
    None, list_of_exps exps
  | Pexp_apply({pexp_desc=Pexp_ident(id); _}, [(_, {pexp_desc=Pexp_tuple(exps); _})]) -> 
    Some (string_of_longident id.txt), list_of_exps exps
  | _ ->
    Location.raise_errorf ~loc "Must be a tuple of roles: %a" Pprintast.expression exp

let to_session_types ~loc roles typ =
  let typs = match typ.ptyp_desc with
    | Ptyp_tuple(typs) -> typs
    | _ -> Location.raise_errorf ~loc:typ.ptyp_loc "Not a tuple type: %a" Pprintast.core_type typ
  in
  if List.length roles <> List.length typs then
    Location.raise_errorf ~loc "role number mismatch: %a" (Format.pp_print_list Format.pp_print_string) roles 
  else begin
    let roletyp = List.map2 (fun x y -> (x,y)) roles typs in
    let rec loop err (acc_sess,acc_err) = function
      | [] -> 
          if err then begin
            Either.Right (List.rev acc_err) (* return errors *)
          end else
            Left (List.rev acc_sess)
      | (role,typ)::xs ->
        begin match to_session_type typ with
        | Left sess ->
          loop (err||false) ((role,sess)::acc_sess, (role,typ)::acc_err) xs
        | Right typ -> 
          (* found an error *)
          loop true ([], (role,typ)::acc_err) xs
        end
    in
    loop false ([],[]) roletyp
  end

let make_error_hole ~loc types =
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

let mark_alert_pat loc pat str : pattern =
  {pat with ppat_attributes=pat.ppat_attributes @ [ppwarning ~loc str]}

let mark_alert_typ loc typ str : core_type =
  {typ with ptyp_attributes=typ.ptyp_attributes @ [ppwarning ~loc str]}

(* XXX *)
let core_type_of_type_expr typ =
  Printtyp.reset_and_mark_loops typ;
  let otyp = Printtyp.tree_of_typexp false typ in
  let typ_str = Format.asprintf "%a" !Oprint.out_type otyp in
  Parse.core_type (Lexing.from_string typ_str)
;;

let project_trace ~msg role (actions:Runkmc.action list) =
  List.fold_left (fun sess Runkmc.{from;to_;mode;label;payload} ->
      let from = Sess.readrole from
      and to_ = Sess.readrole to_ in
      match mode with
      | Out when from=role -> Sess.Out(to_,[(label,(payload,sess))])
      | Inp when to_=role -> Sess.Inp(from,[(label,(payload,sess))])
      | _ -> sess
    ) (Err msg) (List.rev actions)

let report_traces roles (res:Runkmc.kmc_result) =
  let make_traces msg trace =
    List.map (fun role -> role, project_trace ~msg role trace) roles
  in
  if res.progress_violation <> [] then 
    make_traces "progress_violation" (List.hd res.progress_violation) (* XXX FIXME *)
  else if res.eventual_reception_violation <> [] then
    make_traces "eventual_reception_violation" (List.hd res.eventual_reception_violation) (* XXX FIXME *)
  else
    []

let exp_error ~loc msg =
  Ast_helper.Exp.extension ~loc 
    ({Location.txt="ocaml.error";loc}, 
    PStr [{pstr_desc=Pstr_eval(Ast_helper.Exp.constant ~loc (Ast_helper.Const.string msg),[]); 
           pstr_loc=loc}])


type ppx_jrklib_state = {kmc_error_traces: (string, Runkmc.kmc_result) Hashtbl.t}

let transl_kmc_gen_expr
  (state : ppx_jrklib_state)
  (super : Untypeast.mapper) 
  (self : Untypeast.mapper) 
  (exp : Typedtree.expression) =
  match exp.exp_attributes with
  | [{attr_name={txt="kmc.gen"; _};attr_payload=payload; attr_loc=loc}] ->
    begin match payload with 
    | PStr[{pstr_desc=Pstr_eval(rolespec,_);_}] -> 
      let ptyp = core_type_of_type_expr exp.exp_type in
      let sysname, rolespec = rolespec_of_payload ~loc rolespec in
      let sts = to_session_types ~loc rolespec ptyp in
      begin match sts with
      | Right typs -> 
        make_error_hole ~loc typs
      | Left sts -> 
        begin match Runkmc.run sts with
        | () -> 
          let msg = 
            "\nsession types: " ^ 
            String.concat "; " (List.map (fun (role,st) -> showrole role ^ ": " ^ show_sess st) sts)
          in
          let exp = make_chvecs ~loc sts in
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

let transl_kmc_check_pat (super : Untypeast.mapper) (self : Untypeast.mapper) pat =
  let open Typedtree in
  match pat.pat_extra with
  | (Tpat_constraint({ctyp_attributes=([ {attr_name={txt="kmc.check"; _}; attr_loc=loc; _} ] as attrs); _}), extra_loc, _) :: rem ->
    let orig_typ = core_type_of_type_expr pat.pat_type in
    let typ = 
      let typ, msg =
        match to_session_type orig_typ with
        | Left st ->
          orig_typ, show_sess st
        | Right errtyp ->
          errtyp, Format.asprintf "original: %a\nrewritten: %a" Pprintast.core_type orig_typ Pprintast.core_type errtyp      
        in
        mark_alert_typ loc {typ with ptyp_loc=loc; ptyp_attributes=attrs} msg
    in
    let desc = Ppat_constraint(super.pat self {pat with pat_extra=rem}, typ) in
    Ast_helper.Pat.mk ~loc:extra_loc desc
  | _ ->
    super.pat self pat

let projection_of_payload ~loc exp =
  match exp.pexp_desc with
  | Pexp_field({pexp_desc=Pexp_ident({txt=sysname; _}); _}, {txt=rolename; _}) ->
    string_of_longident sysname, string_of_longident rolename
  | _ ->
    (* warning? *)
    Location.raise_errorf ~loc "bad projection specification: %a" Pprintast.expression exp

class insert_kmc_error_traces_as_types (state:ppx_jrklib_state) = object
  inherit Ppxlib.Ast_traverse.map as super

  method! core_type typ =
    match typ.ptyp_attributes with
    | {attr_name={txt="kmc.check"; _}; attr_payload=PStr[{pstr_desc=Pstr_eval(payload,_); _}]; attr_loc; _} :: attr_rem ->
      let g, role = projection_of_payload ~loc:typ.ptyp_loc payload in
      begin match Hashtbl.find_opt state.kmc_error_traces g with
      | Some result ->
        let traces = report_traces [role] result in
        begin match List.assoc_opt role traces with
        | Some st -> 
          let typ = make_chvec_type ~loc:attr_loc st in
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
let make_untyper state = 
  let super = Untypeast.default_mapper in
  {super with 
    expr = (fun self -> transl_kmc_gen_expr state super self);
    pat = (fun self -> transl_kmc_check_pat super self)
  }

let transform str =
  let str = (new replace_holes)#structure str in
  let env = 
    Compmisc.init_path (); 
    Compmisc.initial_env () 
  in
  let (tstr, _, _, _) = Typemod.type_structure env str in
  let state = 
    {kmc_error_traces=Hashtbl.create 42} 
  in
  let untyper = make_untyper state in
  let str = untyper.structure untyper tstr in
  (new insert_kmc_error_traces_as_types state)#structure str


let () = Ppxlib.Driver.register_transformation
  ~impl: transform
  "ppx_jrklib"
