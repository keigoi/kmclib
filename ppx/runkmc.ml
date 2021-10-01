open Sess

exception KMCFail of string

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

let check_output =
  let regex = Str.regexp "^[0-9]+-MC:.*True.*" in
  fun lines ->
  List.exists (fun line ->
    prerr_endline @@ "KMC:" ^ line; 
    Str.string_match regex line 0 || String.trim line = "") lines

let remove_escape_sequence =
  fun str ->
    let regex = Str.regexp "\027\\[[0-9]m" in
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
  else if check_output lines then
    ()
  else
    raise @@ KMCFail(Printf.sprintf "%s\nInput:%s" (String.concat "\n" lines) kmcsrc ^ msg)
    
