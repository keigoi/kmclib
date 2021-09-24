open Ocaml_common

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


class replace_hole = object
  inherit Ppxlib.Ast_traverse.map as super
  method! expression e = match e with
    | {pexp_desc=Pexp_extension({txt="jrklib";loc},_);_} -> 
      [%expr (assert false)[@HOLE]]
    | e -> 
      super#expression e
end

type sess =
    Out of string * (string * cont) list
  | Inp of string * (string * cont) list
  | End
and cont = string * sess
(* [@@deriving show] *)

let rec show_sess = function
  | Out(role,conts) ->
    role ^ "!{" ^ String.concat "; " (List.map show_conts conts) ^ "}"
  | Inp(role,conts) ->
    role ^ "?{" ^ String.concat "; " (List.map show_conts conts) ^ "}"
  | End ->
    "end"
and show_conts (lab,(pld,sess)) =
    lab ^ "<" ^ pld ^ ">" ^ show_sess sess




let rec string_of_out_ident : Outcometree.out_ident -> string = function
  | Oide_apply(_,id) ->  string_of_out_ident id
  | Oide_dot(_,id) -> id
  | Oide_ident {printed_name} -> printed_name

let string_of_otyp = Format.asprintf "%a" !Oprint.out_type

let rec to_session_type = function
  | Outcometree.Otyp_object ([role, Otyp_object (flds, _)], _) ->
    Out(role, List.map (fun (lab, typ) -> lab, output typ) flds)
  | Outcometree.Otyp_object ([role, Otyp_constr(name,[variant])], _)
      when string_of_out_ident name = "inp" ->
    Inp(role, input_variant variant)
  | Otyp_constr(name,[]) when string_of_out_ident name = "unit" ->
    End
  | t -> 
    failwith @@ Format.asprintf "not a session type:%a" !Oprint.out_type t
and output = function
  | Otyp_constr(name,[pld;cont]) when string_of_out_ident name = "out" ->
    string_of_otyp pld, to_session_type cont
  | _ -> failwith "not a continuation"
and input_variant = function
  | Otyp_variant(_,Ovar_fields flds,_,_) ->
    List.map (fun (lab,_,typs) -> 
      if List.length typs = 1 then 
        lab, input_pair @@ List.hd typs 
      else 
        failwith "not a propre variant") flds
  | _ -> failwith "not a variant type"
and input_pair = function
  | Otyp_tuple([pld;cont]) -> string_of_otyp pld, to_session_type cont
  | _ -> failwith "not a pair type"

let mark_alert loc exp exp' : Parsetree.expression =
  let expstr = 
    Format.asprintf "Filled: (%a)" Ocaml_common.Pprintast.expression exp' 
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
    prerr_endline "found hole";
    let otyp = Printtyp.tree_of_typexp false texpr.exp_type in
    let s = show_sess @@ to_session_type otyp in
    let payload = Ast_helper.Exp.constant @@ Ast_helper.Const.string s in
    let open Parsetree in
    let exp = [%expr assert false] in
    Option.some @@ mark_alert loc exp payload
  | _ ->
    None

let transform str =
  let str = (new replace_hole)#structure str in
  let str = make_expr_untyper gen str in
  str


let () = Ppxlib.Driver.register_transformation
  ~impl: transform
  ~intf: (fun i -> i)
  "ppx_jrklib"