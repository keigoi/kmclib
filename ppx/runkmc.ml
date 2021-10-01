open Sess

type mode = Out | Inp[@@deriving show]
type action = 
  {from:string; to_:string; mode:mode; label:string; payload:string}[@@deriving show]
type kmc_result = {
  input : string;
  ksafe : int option;
  progress_violation: action list;
  eventual_reception_violation: action list;
  lines: string list;
}[@@deriving show]

exception KMCFail of string
exception KMCUnsafe of kmc_result

let read_lines filename = 
  let inp = open_in filename in
  let lines = ref [] in
  begin try
    while true; do
      lines := input_line inp :: !lines
    done;
    !lines
  with End_of_file ->
    close_in inp;
    List.rev !lines
  end

let parse_action =
  let regex = Str.regexp "\\([A-Za-z0-9_]+\\)->\\([A-Za-z0-9_]+\\)\\(!\\|?\\)\\([A-Za-z0-9_]+\\)<\\([A-Za-z0-9_]+\\)>" in
  fun action ->
    if Str.string_match regex action 0 then
      let from = Str.matched_group 1 action
      and to_ = Str.matched_group 2 action
      and mode = if Str.matched_group 3 action = "!" then Out else Inp
      and label = Str.matched_group 4 action
      and payload = Str.matched_group 5 action
      in {from;to_;mode;label;payload}
    else
      failwith action

let parse_actions =
  let sep_regex = Str.regexp  "\\(, *\\)\\|\\(; *\\)" in
  fun str ->
    List.map parse_action (Str.split sep_regex str)

let match_a_group regex line =
  if Str.string_match regex line 0 then
    let str = Str.matched_group 1 line in
    Some str
  else
    None

let parse_ptrace =
  let regex = Str.regexp "^Traces violating progress: *\\[\\(.*\\)\\]$" in
  fun line ->
    Option.map parse_actions (match_a_group regex line)
  
let parse_etrace =
  let regex = Str.regexp "^Traces violating eventual reception: *\\[\\(.*\\)\\]$" in
  fun line ->
    Option.map parse_actions (match_a_group regex line)

let parse_kmc =
  let regex = Str.regexp "^\\([0-9]+\\)-MC:.*True.*" in
  fun line ->
    
    Option.map int_of_string @@ match_a_group regex line

let check_output =
  fun lines ->
    let ksafe, ptrace, etrace =
      List.fold_left (fun (safe,ptrace,etrace) line ->
          prerr_endline @@ "KMC:" ^ line; 
          let safe = if safe=None then parse_kmc line else safe
          and ptrace = Option.value (parse_ptrace line) ~default:ptrace
          and etrace = Option.value (parse_etrace line) ~default:etrace
          in
          (safe, ptrace, etrace)
        )
        (None,[],[]) 
        lines
    in
    {input = ""; ksafe; progress_violation=ptrace; eventual_reception_violation=etrace; lines}


let remove_escape_sequence =
  fun str ->
    let regex = Str.regexp "\027\\[[0-9]+m" in
    Str.global_replace regex "" str

let run ?(low=1) ?(hi=20)  (all : (string * Sess.t) list) : unit =
  let infile = Filename.temp_file "jrklib_session" ".txt" in
  let resfile = Filename.temp_file "jrklib_result" ".txt" in
  let kmcsrc = String.concat "\n" @@ List.map (fun (role,sess) -> showrole role ^ ": " ^ show_sess sess) all in
  begin
    let out = open_out_bin infile in
    output_string out kmcsrc;
    close_out out;
  end;
  let cmdline = Printf.sprintf "KMC %s %d %d --debug >%s 2>&1" infile low hi resfile in
  let msg = "\nCommand line: " ^ cmdline in
  let retcode = Sys.command cmdline in
  if not (Sys.file_exists resfile) then
    raise @@ KMCFail (Printf.sprintf "KMC: output not found. retcode:%d" retcode ^ msg);
  let lines = read_lines resfile in
  let lines = List.map remove_escape_sequence lines in
  if retcode <> 0 then
    raise @@ KMCFail (Printf.sprintf "KMC failed with errorcode %d: %s\nInput: %s" retcode (String.concat "\n" lines) kmcsrc ^ msg)
  else 
    let result = check_output lines in
    let result = {result with input = kmcsrc} in
    match result.ksafe with
    | Some _k -> ()
    | None -> raise (KMCUnsafe(result))
    
