open Ppxlib

class my_map = object
  inherit Ast_traverse.map
  method! constant c = match c with
    | Pconst_integer (_, _) -> Pconst_integer ("42", None)
    | _ -> c
end

let () = Ppxlib.Driver.register_transformation
  ~impl: (new my_map)#structure
  ~intf: (fun i -> i)
  "ppx_jrklib"