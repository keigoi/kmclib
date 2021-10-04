
(* reconstruct OCaml type from session types (traces); 
   to indicate the error location using types *)
val make_chvec_type : loc:Warnings.loc -> Sess.t -> Parsetree.core_type

(* translate an OCaml types to a session type.
 * Left: The translated session type
 * Right: Translation error; the error location is marked like [`this_format_is_wrong]
 *)
val to_session_type :
  Parsetree.core_type -> (Sess.t, Parsetree.core_type) Either.t

val to_session_types :
  loc:Warnings.loc ->
  string list -> (* role list*)
  Parsetree.core_type ->
  ((string * Sess.t) list, (string * Parsetree.core_type) list) Either.t
