open Syntax
exception Undefined

let rec eval env = function
  Num n     -> VNum n
| Bool b    -> VBool b
| Var x     -> eval env (Environment.lookup x env)
| ExpSucc m ->
    (match eval env m with
      VNum n  -> VNum (n + 1)
    | _       -> raise Undefined)
| ExpPred m ->
    (match eval env m with
      VNum 0  -> VNum 0
    | VNum n  -> VNum (n - 1)
    | _       -> raise Undefined)
| ExpIsZero m ->
    (match eval env m with
      VNum 0  -> VBool true
    | VNum _  -> VBool false
    | _       -> raise Undefined)
| ExpIf (l, m, n) ->
    (match eval env l with
      VBool true  -> eval env m
    | VBool false -> eval env n
    | _           -> raise Undefined)
| ExpAbs (x, s, m) -> VAbs (x, s, m)
| ExpApp (m, n) ->
    (match eval env m with
      VAbs (x, s, l) -> eval (Environment.extend x n env) l
    | _ -> raise Undefined)
| ExpFix (x, s, m) -> eval (Environment.extend x (ExpFix (x,s,m)) env) m
