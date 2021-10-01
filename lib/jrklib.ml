type ('var, 't) constr = {make_var: 't -> 'var; match_var: 'var -> 't option}

module LabelAndPayload : sig
  type t
  type 'var branch = Branch : ('var, 'v * 't) constr * 't lazy_t -> 'var branch

  val wrap : ('var, 'v * 't) constr -> 'v -> t
  val branch : 'var branch list -> t -> 'var
end = struct
  type t = LabelAndPayload of Obj.t
  type 'var branch = Branch : ('var, 'v * 't) constr * 't lazy_t -> 'var branch

  external compromise : ('var, 'v * 't) constr -> ('var, 'v) constr = "%identity"

  let in_ : 'var 'v. ('var, 'v) constr -> 'v -> t =
    fun {make_var; _} v -> 
      LabelAndPayload (Obj.repr (make_var v))

  let out : 'var 'v. ('var, 'v) constr -> t -> 'v option =
    fun {match_var; _} (LabelAndPayload var) -> 
      match_var (Obj.obj var)

  let wrap : 'var 't. ('var,'v * 't) constr -> 'v -> t =
    fun var v -> in_ (compromise var) v

  let branch : type var. var branch list -> t -> var =
    fun ws lv ->
    let rec loop : var branch list -> var = function
      | [] -> 
        failwith "branch failed"
      | Branch(var,k)::ws ->
        begin match out (compromise var) lv with
        | Some v -> var.make_var (v,Lazy.force k)
        | None -> loop ws
      end
    in
    loop ws
end


type ('v, 's) out = 
  Out : ('var, 'v * 't) constr * LabelAndPayload.t Domainslib.Chan.t * 's lazy_t -> ('v,'s) out

type 'var inp = 
  Inp of LabelAndPayload.t Domainslib.Chan.t * 'var LabelAndPayload.branch list

let send : type v k. (v,k) out -> v -> k =
  fun (Out(var,ch,k)) v ->
    Domainslib.Chan.send ch (LabelAndPayload.wrap var v);
    Lazy.force_val k

let receive : type var. var inp -> var =
  fun (Inp(ch,bs)) ->
    let lv = Domainslib.Chan.recv ch in
    LabelAndPayload.branch bs lv

let close : unit -> unit = fun _ -> ()

module Internal = struct
  type wrapped = LabelAndPayload.t

  let make : 's 't 'var 'v. wrapped Domainslib.Chan.t -> 's -> ('var,'v * 't) constr -> 't -> ('v,'s) out * 'var inp =
    fun ch s var t ->
    Out(var,ch,Lazy.from_val s), Inp(ch,[Branch(var,Lazy.from_val t)])

  let make_lazy : 's 't 'var 'v. wrapped Domainslib.Chan.t -> 's lazy_t -> ('var,'v * 't) constr -> 't lazy_t -> ('v,'s) out * 'var inp =
    fun ch s var t ->
    Out(var,ch,s), Inp(ch,[Branch(var,t)])

  let make_out =
    fun ch var s ->
    Out(var,ch,Lazy.from_val s)

  let make_out_lazy =
    fun ch var s ->
    Out(var,ch,s)

  let make_inp =
    fun ch var t ->
    Inp(ch,[Branch(var,Lazy.from_val t)])

  let make_inp_lazy =
    fun ch var t ->
    Inp(ch,[Branch(var,t)])

  let merge_inp : 'var. 'var inp -> 'var inp -> 'var inp =
    fun (Inp(ch1,bs1)) (Inp(ch2,bs2)) ->
      assert (ch1 == ch2);
      Inp(ch1,bs1@bs2)
end
