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
    Environment.Not_bound x -> "ERROR: Unbound variable: " ^ x
  | Eval.Undefined          -> "undef"
  | Parsing.Parse_error     -> "ERROR: Syntax error"
  | Typing.Typing_error msg -> "ERROR: " ^ msg

let rec read_eval_print_loop _ =
  try
    print_string "# ";
    let buffer = read_line () in
    print_string (eval_and_stringify buffer);
    print_newline ();
    read_eval_print_loop ()
  with
    End_of_file ->
      print_newline ();
      exit 0

let () =
  read_eval_print_loop ()
