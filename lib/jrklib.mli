type ('var, 't) constr = {
  make_var : 't -> 'var;
  match_var : 'var -> 't option;
}

type ('v, 's) out
type 'var inp

val send : ('v, 'k) out -> 'v -> 'k
val receive : 'var inp -> 'var
val close : unit -> unit

(* overload heterogeneous list constructors *)
type _ kmc =
| [] : unit kmc
| (::) : 'a * 'b kmc -> ('a * 'b) kmc

(* and recover the original *)
type 'a list = 'a Stdlib.List.t = 
| [] 
| (::) of 'a * 'a list

type 't spec

val gen : 't spec -> 't kmc

val start_server : ('s * 's_rem) spec -> ('s -> unit) -> Thread.t * 's_rem kmc

module Internal : sig
  type wrapped

  val make : wrapped Domainslib.Chan.t -> 's -> ('var,'v * 't) constr -> 't -> ('v,'s) out * 'var inp
  val make_lazy : wrapped Domainslib.Chan.t -> 's lazy_t -> ('var,'v * 't) constr -> 't lazy_t -> ('v,'s) out * 'var inp
  val make_out : wrapped Domainslib.Chan.t -> ('var,'v * 't) constr -> 's -> ('v,'s) out
  val make_out_lazy : wrapped Domainslib.Chan.t -> ('var,'v * 't) constr -> 's lazy_t -> ('v,'s) out
  val make_inp : wrapped Domainslib.Chan.t -> ('var,'v * 't) constr -> 't -> 'var inp
  val make_inp_lazy : wrapped Domainslib.Chan.t -> ('var,'v * 't) constr -> 't lazy_t -> 'var inp
  val merge_inp : 'var inp -> 'var inp -> 'var inp
  val make_spec : (unit -> 't kmc) -> 't spec
end
