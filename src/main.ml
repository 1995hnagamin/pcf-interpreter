open Syntax

let eval_input buffer =
  let lexbuf = Lexing.from_string buffer in
  let ast = Parser.toplevel Lexer.token lexbuf in
  let Syntax.Exp p = ast in
  let ty = Typing.eval_ty Environment.initial_env p in
  let value = Eval.eval Environment.initial_env p in
  (ty, value)

let eval_and_stringify buffer =
  try
    let (ty, value) = eval_input buffer in
    Printf.sprintf "- %s : %s" (string_of_ty ty) (string_of_value value)
  with
    Eval.Undefined -> "undef"
  | Typing.Typing_error msg -> "Error: " ^ msg 

let rec read_eval_print_loop _ =
  print_string "# ";
  let buffer = read_line () in
  print_string (eval_and_stringify buffer);
  print_newline ();
  read_eval_print_loop ()

let () =
  read_eval_print_loop ()
