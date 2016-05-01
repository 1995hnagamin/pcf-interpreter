open Syntax

let eval_input buffer =
  let lexbuf = Lexing.from_string buffer in
  let ast = Parser.toplevel Lexer.token lexbuf in
  let Syntax.Exp p = ast in
  Eval.eval Environment.initial_env p

let eval_and_stringify buffer =
  try
    string_of_value (eval_input buffer)
  with
    Eval.Undefined -> "undef"

let rec read_eval_print_loop _ =
  print_string "# ";
  let buffer = read_line () in
  print_string (eval_and_stringify buffer);
  print_newline ();
  read_eval_print_loop ()

let () =
  read_eval_print_loop ()
