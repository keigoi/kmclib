open Parsetree

let string_of_ptyp typ =
  let str = Format.asprintf "%a" Pprintast.core_type typ in
  let regex = Str.regexp "[\\(\\)\\* ]" in
  Str.global_replace regex "" str

let handler_errtyp ?(loc = Location.none) msg =
  Ast_helper.Typ.variant ~loc
    [ Ast_helper.Rf.tag { txt = msg; loc = Location.none } false [] ]
    Asttypes.Closed None

type 'x exit = { f : 't. 'x -> 't }

let map_all (exit : _ exit) f =
  let rec loop (acc, visited) = function
    | [] -> List.rev acc
    | x :: xs -> (
      let exit = { f = (fun x -> exit.f (List.rev visited @ (x :: xs))) } in
      match f exit x with
      | Some y -> loop (y :: acc, x :: visited) xs
      | None -> loop (acc, x :: visited) xs)
  in
  loop ([], [])

let rec to_session_type (recvars : string list) =
  let update_exit exit f = { f = (fun t -> exit.f (f t)) } in
  let open Sess in
  (* payload * sess *)
  let payload_cont_pair exit ty =
    match ty.ptyp_desc with
    | Ptyp_tuple [ pld; cont ] ->
      let exit =
        update_exit exit (fun t ->
            { ty with ptyp_desc = Ptyp_tuple [ pld; t ] })
      in
      (string_of_ptyp pld, to_session_type recvars exit cont)
    | _ -> exit.f (handler_errtyp "should_be_a_payload_cont_pair")
  in
  (* payload -> sess *)
  let payload_cont_arrow exit ty =
    match ty.ptyp_desc with
    | Ptyp_arrow (lab, pld, cont) ->
      let exit =
        update_exit exit (fun t ->
            { ty with ptyp_desc = Ptyp_arrow (lab, pld, t) })
      in
      (string_of_ptyp pld, to_session_type recvars exit cont)
    | _ -> exit.f (handler_errtyp "should_be_a_payload_cont_pair")
  in
  (* [`lab of pld * typ] *)
  let rtag exit r =
    match r.prf_desc with
    | Rtag (lab, opt, typs) ->
      if List.length typs = 1 then
        let exit =
          update_exit exit (fun t ->
              { r with prf_desc = Rtag (lab, opt, [ t ]) })
        in
        Some (lab.txt, payload_cont_pair exit (List.hd typs))
      else
        exit.f
          { r with
            prf_desc =
              Rtag (lab, opt, [ handler_errtyp "should_not_be_a_conjunction" ])
          }
    | _ -> None
  in
  (* <lab: pld -> typ> *)
  let otag exit o =
    match o.pof_desc with
    | Otag (lab, typ) ->
      let exit =
        update_exit exit (fun t -> { o with pof_desc = Otag (lab, t) })
      in
      Some (lab.txt, payload_cont_arrow exit typ)
    | _ -> None
  in
  (* <lab: typ> or [`lab of typ] inp *)
  let input_or_output exit role typ =
    match typ.ptyp_desc with
    | Ptyp_object (flds, x) ->
      let exit =
        update_exit exit (fun flds ->
            { typ with ptyp_desc = Ptyp_object (flds, x) })
      in
      Inp (role, map_all exit otag flds)
    | Ptyp_variant (flds, x, y) ->
      let exit =
        update_exit exit (fun flds ->
            { typ with ptyp_desc = Ptyp_variant (flds, x, y) })
      in
      Out (role, map_all exit rtag flds)
    | _ -> exit.f (handler_errtyp "should_be_input_or_output")
  in
  fun exit (ty : core_type) ->
    match ty.ptyp_desc with
    (* [`role of typ] -- input or output *)
    | Ptyp_variant
        ([ ({ prf_desc = Rtag (role, opt, typs); _ } as prf) ], flag, bounds) ->
      let exit =
        update_exit exit (fun t ->
            { ty with
              ptyp_desc =
                Ptyp_variant
                  ( [ { prf with prf_desc = Rtag (role, opt, [ t ]) } ]
                  , flag
                  , bounds )
            })
      in
      input_or_output exit role.Location.txt (List.hd typs)
    (* unit *)
    | Ptyp_constr (name, []) when Util.string_of_longident name.txt = "unit" ->
      End
    (* recursion variable *)
    | Ptyp_var var ->
      if List.mem var recvars then
        Var var
      else
        exit.f (handler_errtyp @@ "unbound_session_" ^ var)
    (* recursion *)
    | Ptyp_alias (t, var) ->
      let exit =
        update_exit exit (fun typ ->
            { ty with ptyp_desc = Ptyp_alias (typ, var) })
      in

      Rec (var, to_session_type (var :: recvars) exit t)
    (* error *)
    | _ -> exit.f (handler_errtyp "should_be_a_role_tag_or_a_unit_type")

exception FormatError of core_type

let runner_type_to_session_type ty =
  let exit = { f = (fun ty -> raise (FormatError ty)) } in
  match ty.ptyp_desc with
  | Ptyp_arrow (lab, argty, retty) -> (
    try Either.Left (to_session_type [] exit argty) with
    | FormatError errty ->
      Either.Right { ty with ptyp_desc = Ptyp_arrow (lab, errty, retty) })
  | _ -> Either.Right (handler_errtyp "should be an arrow type")

let runner_type_to_session_types ~loc roles typ =
  let wrapped, typs =
    match typ.ptyp_desc with
    | Ptyp_tuple typs -> (false, typs)
    | Ptyp_constr ({ txt = id; _ }, [ { ptyp_desc = Ptyp_tuple typs; _ } ])
      when Util.string_of_longident id = "kmctup" ->
      (true, typs)
    | _ ->
      Location.raise_errorf ~loc:typ.ptyp_loc "Not a tuple type: %a"
        Pprintast.core_type typ
  in
  if List.length roles <> List.length typs then
    Location.raise_errorf ~loc "role number mismatch: %a"
      (Format.pp_print_list Format.pp_print_string)
      roles
  else
    let roletyp = List.map2 (fun x y -> (x, y)) roles typs in
    let rec loop err (acc_sess, acc_err) = function
      | [] ->
        if err then
          Either.Right (List.rev acc_err)
        (* return errors *)
        else
          Left (List.rev acc_sess)
      | (role, typ) :: xs -> (
        match runner_type_to_session_type typ with
        | Left sess ->
          loop (err || false)
            ((role, sess) :: acc_sess, (role, typ) :: acc_err)
            xs
        | Right typ ->
          (* found an error *)
          loop true ([], (role, typ) :: acc_err) xs)
    in
    (wrapped, loop false ([], []) roletyp)

let handler_type_to_session_type ty =
  let exit = { f = (fun ty -> raise (FormatError ty)) } in
  try Either.Left (to_session_type [] exit ty) with
  | FormatError errty -> Either.Right errty

let make_handler_type =
  let rec loop = function
    | Sess.Out (role, conts) ->
      let constrs =
        List.map
          (fun (lab, (_typ, cont)) ->
            Ast_helper.Rf.tag
              { txt = lab; loc = Location.none }
              false
              [ Ast_helper.Typ.tuple [ Ast_helper.Typ.any (); loop cont ] ])
          conts
      in
      Ast_helper.Typ.variant
        [ Ast_helper.Rf.tag
            { txt = role; loc = Location.none }
            false
            [ Ast_helper.Typ.variant constrs Asttypes.Open None ]
        ]
        Asttypes.Closed None
    | Inp (role, conts) ->
      let inp conts =
        let flds =
          List.map
            (fun (lab, (_typ, cont)) ->
              Ast_helper.Of.tag
                { txt = lab; loc = Location.none }
                (Ast_helper.Typ.arrow Nolabel (Ast_helper.Typ.any ())
                   (loop cont)))
            conts
        in
        Ast_helper.Typ.object_ flds Asttypes.Open
      in
      Ast_helper.Typ.variant
        [ Ast_helper.Rf.tag
            { txt = role; loc = Location.none }
            false [ inp conts ]
        ]
        Asttypes.Closed None
    | End ->
      Ast_helper.Typ.constr
        { txt = Longident.Lident "unit"; loc = Location.none }
        []
    | Rec (var, st) -> Ast_helper.Typ.alias (loop st) var
    | Var var -> Ast_helper.Typ.var var
    | Err msg -> handler_errtyp msg
  in
  fun ~loc sess ->
    let typ = loop sess in
    { typ with ptyp_loc = loc }
