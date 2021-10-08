open Parsetree

let string_of_ptyp = Format.asprintf "%a" Pprintast.core_type

let handler_errtyp ?(loc=Location.none) msg =
  Ast_helper.Typ.variant ~loc [Ast_helper.Rf.tag {txt=msg;loc=Location.none} false []] Asttypes.Closed None

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
  (* payload * sess *)
  let payload_cont_pair exit ty = match ty.ptyp_desc with
    | Ptyp_tuple([pld;cont]) ->
      let exit =
        {f=fun cont -> exit.f {ty with ptyp_desc = Ptyp_tuple([pld;cont])}} 
      in
      string_of_ptyp pld, to_session_type vars exit cont
    | _ -> exit.f (handler_errtyp "should_be_a_payload_cont_pair")
  in
  (* payload -> sess *)
  let payload_cont_arrow exit ty = match ty.ptyp_desc with
    | Ptyp_arrow(lab,pld,cont) ->
      let exit =
        {f=fun cont -> exit.f {ty with ptyp_desc = Ptyp_arrow(lab,pld,cont)}} 
      in
      string_of_ptyp pld, to_session_type vars exit cont
    | _ -> exit.f (handler_errtyp "should_be_a_payload_cont_pair")
  in  
  (* [`lab of pld * typ] *)
  let rtag exit r = match r.prf_desc with
    | Rtag(lab,opt,typs) ->
      if List.length typs = 1 then 
        let exit = 
          {f=fun typ -> exit.f {r with prf_desc=Rtag(lab,opt,[typ])}} 
        in
        Some (lab.txt, payload_cont_pair exit (List.hd typs))
      else
        exit.f {r with prf_desc=Rtag(lab,opt,[handler_errtyp "should_not_be_a_conjunction"])}
    | _ -> 
      None
  in  
  (* <lab: pld -> typ> *)
  let otag exit o =
    match o.pof_desc with
    | Otag(lab, typ) -> 
      let exit = 
        {f=fun typ -> exit.f {o with pof_desc=Otag(lab, typ)}} 
      in
      Some (lab.txt, payload_cont_arrow exit typ)
    | _ -> None
  in
  (* <lab: typ> or [`lab of typ] inp *)
  let input_or_output exit role typ =
    match typ.ptyp_desc with
    | Ptyp_object (flds, x) ->
      let exit = 
        {f=fun flds-> exit.f {typ with ptyp_desc=Ptyp_object(flds,x)}} 
      in
      Inp(role, map_all exit otag flds)
    | Ptyp_variant (flds,x,y) ->
      let exit = 
        {f=fun flds-> exit.f {typ with ptyp_desc=Ptyp_variant(flds,x,y)}} 
      in
      Out(role, map_all exit rtag flds)
    | _ -> 
      exit.f (handler_errtyp "should_be_inp_or_output_object")
  in
  fun exit (ty:core_type) -> match ty.ptyp_desc with
  (* [`role of typ] -- input or output *)
  | Ptyp_variant ([{prf_desc=Rtag(role,opt,typs);_} as prf], flag, bounds) ->
    let exit = 
      {f=fun t -> exit.f {ty with ptyp_desc=Ptyp_variant([{prf with prf_desc=Rtag(role,opt,[t])}],flag,bounds)}}
    in
    input_or_output exit role.Location.txt (List.hd typs)
  (* unit *)
  | Ptyp_constr (name, [])
      when Util.string_of_longident name.txt = "unit" ->
    End
  (* recursion variable *)
  | Ptyp_var var ->
    if List.mem var vars then
      Var var
    else
      exit.f (handler_errtyp @@ "unbound_type_variable_" ^ var)
  (* recursion *)
  | Ptyp_alias (t,var) ->
    let exit =
      {f=fun typ -> exit.f {ty with ptyp_desc=Ptyp_alias(typ,var)}}
    in
    Rec(var, to_session_type (var::vars) exit t)
  (* error *)
  | _ -> 
    exit.f (handler_errtyp "should_be_a_role_object_or_a_unit_type")

exception FormatError of core_type

let to_session_type ty = 
  let exit =
    {f=fun ty -> raise (FormatError ty)}
  in
  match ty.ptyp_desc with
  | Ptyp_arrow(lab,argty,retty) ->
    begin try
      Either.Left (to_session_type [] exit argty)
    with
      FormatError(errty) ->
        Either.Right ({ty with ptyp_desc=Ptyp_arrow(lab,errty,retty)})
    end
  | _ ->
    Either.Right (handler_errtyp "should be an arrow type")


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
        | Right typ -> 
          (* found an error *)
          loop true ([], (role,typ)::acc_err) xs
        end
    in
    wrapped, loop false ([],[]) roletyp
  end
