open Ocaml_common

type t =
    Out of string * (string * cont) list
  | Inp of string * (string * cont) list
  | End
  | Rec of string * t
  | Var of string
and cont = payload * t
and payload = string

let rec show_sess = function
  | Out(role,conts) ->
    role ^ "!{" ^ String.concat "; " (List.map show_conts conts) ^ "}"
  | Inp(role,conts) ->
    role ^ "?{" ^ String.concat "; " (List.map show_conts conts) ^ "}"
  | End ->
    "end"
  | Rec(var,t) ->
    "rec " ^ var ^ "." ^ show_sess t
  | Var var -> var
and show_conts (lab,(pld,sess)) =
    lab ^ "<" ^ pld ^ ">." ^ show_sess sess

let new_env () =
  Compmisc.init_path (); 
  Compmisc.initial_env () 

let make_expr_untyper f str =
  let env = new_env () in
  let (tstr, _, _, _) = Typemod.type_structure env str in
  let super = Untypeast.default_mapper in
  let f' self exp =
    match f exp with
    | Some exp -> exp
    | None -> super.expr self exp
  in
  let untyper = {super with expr = f'} in
  untyper.structure untyper tstr  

let fresh_var =
  let cnt = ref 0 in
  fun () ->
  let s = "jrklib_tyvar" ^ string_of_int !cnt in
  cnt := !cnt + 1;
  s

class replace_hole = object
  inherit Ppxlib.Ast_traverse.map as super
  method! expression e = match e with
    | {pexp_desc=Pexp_extension({txt="jrklib";loc},_);_} ->
      let any = Ast_helper.Typ.var (fresh_var ()) in
      [%expr ((assert false)[@HOLE] : [%t any]) ]
    | e ->
      super#expression e
end

let rec string_of_out_ident : Outcometree.out_ident -> string = function
  | Oide_apply(_,id) ->  string_of_out_ident id
  | Oide_dot(_,id) -> id
  | Oide_ident {printed_name} -> printed_name

let string_of_otyp = Format.asprintf "%a" !Oprint.out_type

let var_pat ?(loc=Location.none) varname =
  Ast_helper.Pat.var ~loc {txt=varname;loc}

let var_exp ?(loc=Location.none) varname =
  Ast_helper.Exp.ident ~loc {txt=Longident.Lident varname;loc}

let constr ?(loc=Location.none) : string -> Parsetree.expression = fun label ->
  let open Parsetree in
  let pat constr = Ast_helper.Pat.variant constr (Some (var_pat "x")) in
  let exp constr = Ast_helper.Exp.variant constr (Some (var_exp "x")) in
  [%expr {match_var=(function [%p pat label] -> Some x | _ -> None); make_var=(fun x -> [%e exp label])}]

let method_ ?(loc=Location.none) name exp =
  Ast_helper.Cf.method_ ~loc
    (Location.mkloc name loc) 
    Public 
      (Cfk_concrete(Fresh, exp))

let tycon ?(loc=Location.none) name =
  Ast_helper.Typ.constr {txt=(Longident.Lident name);loc} []

let make_out ?(loc=Location.none) ch label payload cont =
  let open Parsetree in
  [%expr (Internal.make_out_lazy [%e ch] [%e constr ~loc label] [%e cont] : ([%t tycon ~loc payload],_) out)]

let make_inp ?(loc=Location.none) ch label payload cont =
  let open Parsetree in
  let constr = constr ~loc label in
  let payload = tycon ~loc payload in
  let polyvar = Ast_helper.Typ.variant [Ast_helper.Rf.tag {txt=label;loc} false [[%type: [%t payload] * _ ]]] Asttypes.Open None in
  [%expr (Internal.make_inp_lazy [%e ch] [%e constr] [%e cont] : [%t polyvar] inp) ]

let make_object ?(loc=Location.none) methods =
  Ast_helper.Exp.object_ @@ Ast_helper.Cstr.mk [%pat? _] methods

let meta_fold_left_ ?(loc=Location.none) f_meta exps =
  let open Parsetree in
  List.fold_left (fun e1 e2 -> [%expr [%e f_meta] [%e e1] [%e e2]]) (List.hd exps) (List.tl exps)

let let_insert ?(loc=Location.none) bindings exp =
  let open Parsetree in
  List.fold_left (fun body (var,exp) -> 
      [%expr let [%p var_pat var] = [%e exp] in [%e body]]) 
    exp 
    bindings

let letrec ?(loc=Location.none) bindings exp =
  Ast_helper.Exp.let_ ~loc
    Asttypes.Recursive
    (List.map (fun (var,exp) -> Ast_helper.Vb.mk ~loc (var_pat ~loc var) exp) bindings)
    exp

let lazy_ ?(loc=Location.none) exp = 
  let open Parsetree in
  [%expr lazy [%e exp]]

let make_chvec ?(loc=Location.none) = 
  let bindings = ref [] in
  let rec loop ch = function
    | Out(role,conts) ->
      let outs = 
        List.map (fun (label, (payload, cont)) -> 
            let cont = loop ch cont in
            (label, make_out ~loc ch label payload cont)
          ) conts
      in
      lazy_ @@
      let_insert ~loc outs @@
        make_object ~loc @@
          [method_ ~loc role @@ make_object ~loc @@
            List.map (fun (label, _) -> method_ ~loc label (var_exp label)) outs]
    | Inp(role,conts) ->
      lazy_ @@
      make_object ~loc @@ 
        [method_ ~loc role @@
          meta_fold_left_ ~loc [%expr Internal.merge_inp] @@ 
            List.map (fun (label, (payload,cont)) -> 
              let cont = loop ch cont in
              make_inp ~loc ch label payload cont) conts]
    | End ->
      [%expr Lazy.from_val ()]
    | Rec(var,st) ->
      let exp = loop ch st in
      bindings := (var,exp) :: !bindings;
      var_exp var
    | Var var ->
      var_exp var
  in
  fun st ->
  let open Parsetree in
  let exp = loop [%expr ch] st in
  let exp = List.fold_left (fun exp (var,_) -> [%expr ignore (Lazy.force [%e var_exp var]); [%e exp]]) [%expr Lazy.force_val [%e exp]] !bindings in
  let exp = letrec ~loc !bindings exp in
  let_insert [("ch", [%expr Domainslib.Chan.make_unbounded ()])] exp
  

let rec to_session_type =
  let rec output = function
    | Outcometree.Otyp_constr(name,[pld;cont]) when string_of_out_ident name = "out" ->
      string_of_otyp pld, to_session_type cont
    | _ -> failwith "not a continuation"
  and input_variant = function
    | Outcometree.Otyp_variant(_,Ovar_fields flds,_,_) ->
      List.map (fun (lab,_,typs) -> 
        if List.length typs = 1 then 
          lab, input_pair @@ List.hd typs 
        else
          failwith "not a proper variant") flds
    | _ -> failwith "not a variant type"
  and input_pair = function
    | Outcometree.Otyp_tuple([pld;cont]) -> string_of_otyp pld, to_session_type cont
    | _ -> failwith "not a pair type"
  in
  function
  | Outcometree.Otyp_object ([role, Otyp_object (flds, _)], _) ->
    Out(role, List.map (fun (lab, typ) -> lab, output typ) flds)
  | Outcometree.Otyp_object ([role, Otyp_constr(name,[variant])], _)
      when string_of_out_ident name = "inp" ->
    Inp(role, input_variant variant)
  | Otyp_constr(name,[]) 
      when string_of_out_ident name = "unit" ->
    End
  | Otyp_var(_,var) ->
    Var var
  | Otyp_alias(t,var) ->
    Rec(var, to_session_type t)
  | t -> 
    failwith @@ Format.asprintf "not a session type:%a" !Oprint.out_type t

let spec_type : Outcometree.out_type -> Outcometree.out_type = function
  | Otyp_constr(name,[t]) when string_of_out_ident name = "spec" ->
    t
  | _ -> failwith "not a type constructor"

let mark_alert loc exp exp' : Parsetree.expression =
  let expstr = 
    Format.asprintf "Filled: (%a)" Pprintast.expression exp' 
  in
  let payload : Parsetree.expression = 
    Ast_helper.Exp.constant (Ast_helper.Const.string expstr)
  in
  let attr = {
    Parsetree.attr_name={txt="ppwarning";loc=Location.none}; 
    attr_payload=PStr[{pstr_desc=Pstr_eval(payload,[]); pstr_loc=loc}]; 
    attr_loc=Location.none
    } 
  in
  {exp with pexp_attributes=attr::exp.Parsetree.pexp_attributes}

let gen (texpr:Typedtree.expression) =
  match texpr with
  | {exp_attributes=[{attr_name={txt="HOLE";loc};_}]; _} ->
    Printtyp.reset_and_mark_loops texpr.exp_type;
    let otyp = Printtyp.tree_of_typexp false texpr.exp_type in
    let st = to_session_type otyp in
    let exp = make_chvec st in
    let expstr = Format.asprintf "%a" Pprintast.expression exp in
    let msg = show_sess st ^ ";\n" ^ expstr in
    prerr_endline msg;
    let payload = Ast_helper.Exp.constant @@ Ast_helper.Const.string msg in
    Option.some @@ mark_alert loc exp payload
  | _ ->
    None

let transform str =
  let str = (new replace_hole)#structure str in
  let str = make_expr_untyper gen str in
  str


let () = Ppxlib.Driver.register_transformation
  ~impl: transform
  "ppx_jrklib"
