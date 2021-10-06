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

let var_exp ?(loc=Location.none) varname =
  Ast_helper.Exp.ident ~loc {txt=Longident.Lident varname;loc}

let var_pat ?(loc=Location.none) varname =
  Ast_helper.Pat.var ~loc {txt=varname;loc}

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

let make_handler ?(loc=Location.none) (tbl:tbl) self = 
  let open Ast_helper in
  let rec loop handlerexp = function
    | Out(role,conts) ->
      (*
        match handlerexp with
        | `role(`label1(pld,cont_handler)) ->
          Chan.send ch (`label1(v)); run cont_handler
        | .. 
        | `role(`labelN(pld,cont_handler)) ->
          Chan.send ch (`labelN(v)); run cont_handler
      *)
      let role = Sess.readrole role in
      let ch = 
        let ch, _ = get_or_make tbl (RolePair (self,role)) (make_channel (self, role)) in
        Exp.ident ~loc {txt=Longident.Lident ch;loc} 
      in
      let mkcase (label, (_pld, sess)) =
        let pat = 
          (* `role(`label(pld, cont_handler)) -> *)
          (Pat.variant ~loc role 
          (Some (Pat.variant ~loc label 
            (Some [%pat? (pld, cont_handler)]))))
        and exp = 
          (* Chan.send ch (`label(v)); run cont_handler *)
          let raw_payload = 
            (* `label(pld) *)
            Exp.variant ~loc label (Some [%expr pld])
          in
          let cont =
            (* run cont_handler *)
            loop [%expr cont_handler] sess
          in
          [%expr Domainslib.Chan.send [%e ch] (Obj.repr [%e raw_payload]); [%e cont]]
        in
        Exp.case pat exp
      in
      Exp.match_ ~loc 
        handlerexp
        (List.map mkcase conts)
    | Inp(role,conts) ->
      let role = Sess.readrole role in
      let ch, _ = get_or_make tbl (RolePair (role,self)) (make_channel (role,self)) in
      let ch = Exp.ident ~loc {txt=Longident.Lident ch;loc} in
      (*
        match handlerexp with
        | `role(handler) ->
          begin match Obj.obj (Chan.recv ch) with
          | `label1(pld) ->
            run (handler#label1 pld)
          | ..
          | `labelN(pld) ->
            run (handler#labelN pld)
          end
      *)
      let mk_receive_case (label, (_, cont)) =
        let pat = 
          (* `label(pld) *)
          Pat.variant ~loc label
            (Some (Pat.var ~loc {txt="pld";loc}))
        in
        let exp =
          (* run (handler#label pld) *)
          loop [%expr [%e Exp.send ~loc [%expr handler] {txt=label;loc}] pld] cont
        in
        Exp.case pat exp
      in
      Exp.match_ ~loc 
        handlerexp
        [Exp.case 
          (Pat.variant ~loc role (Some [%pat? handler]))
          (Exp.match_ ~loc
            [%expr Obj.obj (Domainslib.Chan.recv [%e ch])]
            (List.map mk_receive_case conts))]
    | End ->
      [%expr match [%e handlerexp] with () -> ()]
    | Rec(var,st) ->
      let exp = [%expr (fun handler -> [%e loop [%expr handler] st])] in
      letrec ~loc [(var, exp)] [%expr [%e var_exp var] [%e handlerexp]]
    | Var var ->
      [%expr [%e var_exp var] [%e handlerexp]]
    | Err msg ->
      Location.raise_errorf ~loc "Impossible: Trying to generate an erroneous channel vector %s" msg
  in
  fun st ->
    [%expr (fun handler -> [%e loop [%expr handler] st])]

let make_handlers ~loc sts =
  let tbl = Hashtbl.create 42 in
  let exps = List.map (fun (role,st) -> make_handler ~loc tbl role st) sts in
  let exp = Ast_helper.Exp.tuple exps ~loc in
  let exp = let_tbl ~loc tbl exp in
  exp
