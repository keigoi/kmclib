type ('var, 't) constr = {
  make_var : 't -> 'var;
  match_var : 'var -> 't option;
}
type ('v, 's) out
type 'var inp


val send : ('v, 'k) out -> 'v -> 'k
val receive : 'var inp -> 'var

module Internal : sig
  type wrapped
  type 'var branch = Branch : ('var, 'v * 't) constr * 't -> 'var branch

  val make : wrapped Domainslib.Chan.t -> 's -> ('var,'v * 't) constr * 't -> ('v,'s) out * 'var inp
  val merge_inp : 'var inp -> 'var inp -> 'var inp
end
