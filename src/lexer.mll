{
let reserverWords = [
  ("num",  Parser.NUMTY);
  ("bool", Parser.BOOLTY);
  ("true", Parser.BOOLV true);
  ("false", Parser.BOOLV false);
  ("succ", Parser.SUCC);
  ("pred", Parser.PRED);
  ("iszero", Parser.ISZERO);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("else", Parser.ELSE);
  ("lambda", Parser.LAMBDA);
  ("mu", Parser.MU);
]

let maybe_assoc default key alist =
  try
    List.assoc key alist
  with
    Not_found -> default
}

rule token = parse
  [' ' '\009' '\012']+ { token lexbuf }
| ['0'-'9']+
  { Parser.NUMV (int_of_string (Lexing.lexeme lexbuf)) }
| "("  { Parser.LPAREN }
| ")"  { Parser.RPAREN }
| "->" { Parser.ARROW }
| "."  { Parser.DOT }
| ":"  { Parser.COLON }
| ['a'-'z']['a'-'z' '0'-'9' '_']*
  { let id = Lexing.lexeme lexbuf in maybe_assoc (Parser.ID id) id reserverWords }
| '\n' { Parser.EOL }
| eof  { Parser.EOL }
