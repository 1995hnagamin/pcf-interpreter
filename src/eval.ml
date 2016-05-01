open Syntax
exception Undefined

let rec substitute x n = function
  Var y       -> if x = y then n else Var y
| ExpSucc m   -> ExpSucc (substitute x n m)
| ExpPred m   -> ExpPred (substitute x n m)
| ExpIsZero m -> ExpIsZero (substitute x n m)
| ExpIf (p, q, r) ->
    let p = substitute x n p in
    let q = substitute x n q in
    let r = substitute x n r in
    ExpIf (p, q, r)
| ExpAbs (y, s, m) ->
    let m' = if x = y then m else substitute x n m in
    ExpAbs (y, s, m')
| ExpApp (p, q) ->
    let p = substitute x n p in
    let q = substitute x n q in
    ExpApp (p, q)
| ExpFix (y, s, m) ->
    let m' = if x = y then m else substitute x n m in
    ExpFix (y, s, m')
| m -> m

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
      VAbs (x, s, l) ->
        let m = substitute x n l in
        eval env m
    | _ -> raise Undefined)
| ExpFix (x, s, m) ->
    let m = substitute x (ExpFix (x, s, m)) m in
    eval env m
