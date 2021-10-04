
let fresh_var =
  let cnt = ref 0 in
  fun ?(prefix="kmclib_var_") ?(suffix="") () ->
  let s = prefix ^ string_of_int !cnt ^ suffix in
  cnt := !cnt + 1;
  s

let rec string_of_longident : Longident.t -> string = function
  | Lapply(_,id) -> string_of_longident id
  | Ldot(_,id) -> id
  | Lident(id) -> id
