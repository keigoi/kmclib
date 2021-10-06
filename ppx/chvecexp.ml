open Parsetree
open Sess

type key = RolePair of string * string | Label of string
type tbl = (key, string * expression) Hashtbl.t

let get_or_make (tbl:tbl) key f =
  match Hashtbl.find_opt tbl key with
  | Some v -> v
  | None ->
    let v = f () in
    Hashtbl.add tbl key v;
    v

let method_ ?(loc=Location.none) name exp =
  Ast_helper.Cf.method_ ~loc
    (Location.mkloc name loc) 
    Public 
      (Cfk_concrete(Fresh, exp))

let var_exp ?(loc=Location.none) varname =
  Ast_helper.Exp.ident ~loc {txt=Longident.Lident varname;loc}

let var_pat ?(loc=Location.none) varname =
  Ast_helper.Pat.var ~loc {txt=varname;loc}

let make_label ?(loc=Location.none) : string -> string * expression = fun label ->
  let pat constr = Ast_helper.Pat.variant constr (Some (var_pat "x")) in
  let exp constr = Ast_helper.Exp.variant constr (Some (var_exp "x")) in
  let var = Util.fresh_var ~prefix:("lab_"^label) () in
  var, [%expr {match_var=(function [%p pat label] -> Some x | _ -> None); make_var=(fun x -> [%e exp label])}]

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
  let var = Util.fresh_var ~prefix:("ch") ~suffix:("_"^r1^"_"^r2) () in
  (var, [%expr Domainslib.Chan.make_unbounded ()])

let make_chvec ?(loc=Location.none) (tbl:tbl) self = 
  let rec loop = function
    | Out(role,conts) ->
      let ch, _ = get_or_make tbl (RolePair (self,role)) (make_channel (self,role)) in
      let out_bindings = 
        List.map (fun (label, (payload, cont)) -> 
            let cont = loop cont in
            (label, make_out ~loc ~tbl (var_exp ch) label payload cont)
          ) conts
      in
      lazy_val @@
      let_ ~loc out_bindings @@
        make_object ~loc @@
          [method_ ~loc role @@ make_object ~loc @@
            List.map (fun (label, _) -> method_ ~loc label (var_exp label)) out_bindings]
    | Inp(role,conts) ->
      let ch, _ = get_or_make tbl (RolePair (role,self)) (make_channel (role,self)) in
      let inps =
        meta_fold_left_ ~loc [%expr Internal.merge_inp] @@ 
            List.map (fun (label, (payload,cont)) -> 
              let cont = loop cont in
              make_inp ~loc ~tbl (var_exp ch) label payload cont) conts
      in
      let var = Util.fresh_var() in
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
