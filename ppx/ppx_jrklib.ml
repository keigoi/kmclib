open Ocaml_common

type key = RolePair of string * string | Label of string
type tbl = (key, string * Parsetree.expression) Hashtbl.t

let get_or_make (tbl:tbl) key f =
  match Hashtbl.find_opt tbl key with
  | Some v -> v
  | None ->
    let v = f () in
    Hashtbl.add tbl key v;
    v

type t =
    Out of string * (string * cont) list
  | Inp of string * (string * cont) list
  | End
  | Rec of string * t
  | Var of string
and cont = payload * t
and payload = string

let showrole s = String.capitalize_ascii s

let rec show_sess = function
  | Out(role,[cont]) ->
    show_conts role "!" cont
  | Inp(role,[cont]) ->
    show_conts role "?" cont
  | Out(role,conts) ->
    "{" ^ String.concat ", " (List.map (show_conts role "!") conts) ^ "}"
  | Inp(role,conts) ->
    "{" ^ String.concat ", " (List.map (show_conts role "?") conts) ^ "}"
  | End ->
    "end"
  | Rec(var,t) ->
    "rec t" ^ var ^ " . " ^ show_sess t
  | Var var -> "t" ^ var
and show_conts role act (lab,(pld,sess)) =
    showrole role ^ act ^ lab ^ "<" ^ pld ^ ">;" ^ show_sess sess

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
  fun ?(prefix="jrklib_var_") ?(suffix="") () ->
  let s = prefix ^ string_of_int !cnt ^ suffix in
  cnt := !cnt + 1;
  s

class replace_hole = object
  inherit Ppxlib.Ast_traverse.map as super
  method! expression e = 
    let open Parsetree in
    match e with
    | {pexp_desc=Pexp_extension({txt="jrklib";loc},payload);_} ->
      let any = Ast_helper.Typ.var (fresh_var ()) in
      let exp = [%expr assert false] in
      let exp = {exp with pexp_attributes=[{attr_name={txt="MAKE_SESS";loc}; attr_payload=payload; attr_loc=loc}]} in
      let exp = [%expr ([%e exp] : [%t any])] in
      exp
    | e ->
      super#expression e
end

let rec string_of_out_ident : Outcometree.out_ident -> string = function
  | Oide_apply(_,id) ->  string_of_out_ident id
  | Oide_dot(_,id) -> id
  | Oide_ident {printed_name} -> printed_name

let rec string_of_longident : Longident.t -> string = function
  | Lapply(_,id) -> string_of_longident id
  | Ldot(_,id) -> id
  | Lident(id) -> id

let string_of_otyp = Format.asprintf "%a" !Oprint.out_type

let var_pat ?(loc=Location.none) varname =
  Ast_helper.Pat.var ~loc {txt=varname;loc}

let var_exp ?(loc=Location.none) varname =
  Ast_helper.Exp.ident ~loc {txt=Longident.Lident varname;loc}

let make_label ?(loc=Location.none) : string -> string * Parsetree.expression = fun label ->
  let open Parsetree in
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
  let open Parsetree in
  let constr, _ = get_or_make tbl (Label label) (fun () -> make_label ~loc label) in
  [%expr (Internal.make_out_lazy [%e ch] [%e var_exp constr] [%e cont] : ([%t tycon ~loc payload],_) out)]

let make_inp ?(loc=Location.none) ~(tbl:tbl) ch label payload cont =
  let open Parsetree in
  let constr, _ = get_or_make tbl (Label label) (fun () -> make_label ~loc label) in
  let payload = tycon ~loc payload in
  let polyvar = Ast_helper.Typ.variant [Ast_helper.Rf.tag {txt=label;loc} false [[%type: [%t payload] * _ ]]] Asttypes.Open None in
  [%expr (Internal.make_inp_lazy [%e ch] [%e var_exp constr] [%e cont] : [%t polyvar] inp) ]

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

let lazy_val ?(loc=Location.none) exp = 
  let open Parsetree in
  [%expr Lazy.from_val [%e exp]]

let make_channel ?(loc=Location.none) (r1,r2) () =
  let open Parsetree in
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
      let_insert ~loc outs @@
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
      let_insert ~loc [(var,inps)] @@
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
  in
  fun st ->
    let exp = loop st in
    let open Parsetree in
    [%expr Lazy.force_val [%e exp]]

let make_chvecs ~loc sts =
  let open Parsetree in
  let roles = List.map fst sts in
  let tbl = Hashtbl.create 42 in
  let exps = List.map (fun (role,st) -> make_chvec tbl role st) sts in
  let exp =  Ast_helper.Exp.tuple exps ~loc in
  let exp = 
    Hashtbl.fold 
      (fun key (var,exp) body -> 
        begin match key with
        | RolePair(from,to_) ->
          if not (List.mem from roles && List.mem to_ roles) then
            failwith (Printf.sprintf "role pair not found: %s,%s" from to_)
        | _ -> ()
        end;
        [%expr let [%p var_pat var] = [%e exp] in [%e body]]
      )
      tbl exp in
  exp

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


let tup_to_list = function
  | Parsetree.{pexp_desc=Pexp_tuple(exps); _} ->
    List.map (function Parsetree.{pexp_desc=Pexp_ident(id); _} -> string_of_longident id.txt | _ -> failwith "not a variable") exps
  | _ -> failwith "not a tuple"

let to_session_types roletups typs =
  let roles = tup_to_list roletups in
  let typs = match typs with
    | Outcometree.Otyp_tuple(typs) -> typs
    | _ -> failwith "not a tuple type"
  in
  if List.length roles <> List.length typs then
    failwith "role number differs"
  else
    List.map2 (fun role typ -> role, to_session_type typ) roles typs

let mark_alert loc exp str : Parsetree.expression =
  let payload : Parsetree.expression = 
    Ast_helper.Exp.constant (Ast_helper.Const.string str)
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
  | {exp_attributes=[{attr_name={txt="MAKE_SESS";loc};attr_payload=mksess; _}]; _} ->
    begin match mksess with 
    | PStr[{pstr_desc=Pstr_eval(rolespec,_);_}] -> 
      Printtyp.reset_and_mark_loops texpr.exp_type;
      let otyp = Printtyp.tree_of_typexp false texpr.exp_type in
      let sts = to_session_types rolespec otyp in
      let exp = make_chvecs ~loc sts in
      let expstr = Format.asprintf "Filled: (%a)" Pprintast.expression exp in
      let msg = "session types: " ^ String.concat "; " (List.map (fun (role,st) -> "role " ^ showrole role ^ ": " ^ show_sess st) sts) ^ ";\n" ^ expstr in
      Option.some @@ mark_alert loc exp msg
    | PStr p -> failwith @@ Format.asprintf "%a" Pprintast.structure p
    | _ -> failwith "payload format not applicable"
    end
  | _ ->
    None

let transform str =
  let str = (new replace_hole)#structure str in
  let str = make_expr_untyper gen str in
  str


let () = Ppxlib.Driver.register_transformation
  ~impl: transform
  "ppx_jrklib"
