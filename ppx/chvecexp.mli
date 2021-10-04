(* generate channel vectors from session types *)
val make_chvecs :
  loc:Warnings.loc -> (string * Sess.t) list -> Parsetree.expression
