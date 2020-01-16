open Parsing

type runningMode = EV | TC | TOK | FULL | HELP

let usage = "Usage: main [ -h | -eval | -tc | -tok ] filename\n"

let parse lexbuf =
  try Parser.prog Lexer.read lexbuf
  with Parser.Error -> failwith "I can't parse this code"

let main rmode fname =
  let inx = open_in fname in
  let lexbuf = Lexing.from_channel inx in
  begin [@warning "-40"]
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
    match rmode with
    | HELP -> print_string usage; exit 1;
    | TOK -> Pretty.prettyTokens lexbuf; exit 1;
    | _ -> ();
    let e = parse lexbuf in
    Printf.printf "Parsed correctly!\n";
    close_in inx;
    match rmode with
    | EV -> Eval.start_program e
    | TC -> Formchecker.wellformed e;
    | FULL -> 
    begin
      Formchecker.wellformed e;
      Eval.start_program e
    end 
    | _ -> ()
  end
  
let getMode s =
  match s with
    | "-h" -> HELP
    | "--help" -> HELP
    | "-eval" -> EV
    | "-tc" -> TC
    | "-tok" -> TOK
    | "-full" -> FULL
    | _ -> Printf.printf "Unrecognized flag: %s\n" s; exit 1

let () =
  let n = Array.length Sys.argv in
  match n with
  | 2 -> main FULL Sys.argv.(1)
  | 3 -> main (getMode Sys.argv.(1)) Sys.argv.(2)
  | _ -> print_string usage; exit 1
