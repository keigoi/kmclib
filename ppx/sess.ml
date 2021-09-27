
type t =
    Out of string * (string * cont) list
  | Inp of string * (string * cont) list
  | End
  | Rec of string * t
  | Var of string
and cont = string * t (* payload * continuation *)

let showrole s = String.capitalize_ascii s

let rec show_sess = function
  | Out(role,[cont]) ->
    show_conts role "!" cont
  | Inp(role,[cont]) ->
    show_conts role "?" cont
  | Out(role,conts) ->
    "{" ^ String.concat ", " (List.map (show_conts role "!") conts) ^ "}"
  | Inp(role,conts) ->
    "{" ^ String.concat ", " (List.map (show_conts role "?") conts) ^ "}"
  | End ->
    "end"
  | Rec(var,t) ->
    "rec t" ^ var ^ " . " ^ show_sess t
  | Var var -> "t" ^ var
and show_conts role act (lab,(pld,sess)) =
    showrole role ^ act ^ lab ^ "<" ^ pld ^ ">;" ^ show_sess sess
