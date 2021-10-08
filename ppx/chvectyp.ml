open Parsetree

let string_of_ptyp = Format.asprintf "%a" Pprintast.core_type

let chvec_errtyp ?(loc=Location.none) msg =
Ast_helper.Typ.variant ~loc [Ast_helper.Rf.tag {txt=msg;loc=Location.none} false []] Asttypes.Closed None

let make_chvec_type ~loc =
  let out cont =
    Ast_helper.Typ.constr ~loc 
      {txt=Longident.Lident "out"; loc} 
      [Ast_helper.Typ.any ~loc (); cont]
  in
  let rec loop = function
  | Sess.Out(role,conts) ->
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
  let open Sess in
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
      (* ignore *)
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
    | Ptyp_constr(name,[pld;cont]) when Util.string_of_longident name.txt = "out" ->
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
  let method_or_inp exit role typ =
    match typ.ptyp_desc with
    | Ptyp_object (flds, x) ->
      let exit = 
        {f=fun flds-> exit.f {typ with ptyp_desc=Ptyp_object(flds,x)}} 
      in
      Out(role.Location.txt, 
          map_all exit otag flds)
    | Ptyp_constr (name, [variant]) when Util.string_of_longident name.txt = "inp" ->
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
    method_or_inp exit role typ
  (* unit *)
  | Ptyp_constr (name, [])
      when Util.string_of_longident name.txt = "unit" ->
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


let to_session_types ~loc roles typ =
  let wrapped, typs = match typ.ptyp_desc with
    | Ptyp_tuple(typs) -> 
      false, typs
    | Ptyp_constr({txt=id;_}, [{ptyp_desc=Ptyp_tuple(typs);_}]) when Util.string_of_longident id = "kmctup" -> 
      true, typs
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
        | Right typ' -> 
          (* found an error *)
          prerr_endline @@ Format.asprintf "%a" Pprintast.core_type typ;
          loop true ([], (role,typ')::acc_err) xs
        end
    in
    wrapped, loop false ([],[]) roletyp
  end
