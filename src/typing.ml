open Syntax

exception Typing_error of string

let rec eval_ty env = function
  Num _     -> TyNum
| Bool _    -> TyBool
| Var x     -> Environment.lookup x env
| ExpSucc m ->
    (match eval_ty env m with
      TyNum -> TyNum
    | _     -> raise (Typing_error "Not a number"))
| ExpPred m ->
    (match eval_ty env m with
      TyNum -> TyNum
    | _     -> raise (Typing_error "Not a number"))
| ExpIsZero m ->
    (match eval_ty env m with
      TyNum -> TyBool
    | _     -> raise (Typing_error "Not a number"))
| ExpIf (l, m, n) ->
    (match eval_ty env l with
      TyBool ->
        let ty1 = eval_ty env m in
        let ty2 = eval_ty env n in
        if ty1 = ty2
        then ty1
        else raise (Typing_error "Then and Else clause don't match")
    | _ -> raise (Typing_error "Not a boolean in predicate clause"))
| ExpApp (m, n) ->
    let ty1 = eval_ty env m in
    let ty2 = eval_ty env n in
    (match ty1 with
      TyFun (s, t) ->
        if s = ty2
        then t
        else raise (Typing_error "Invalid application")
    | _ -> raise (Typing_error "Not a function"))
| ExpAbs (x, s, m) ->
    let env' = Environment.extend x s env in
    let ty'  = eval_ty env' m in
    TyFun (s, ty')
| ExpFix (x, s, m) ->
    let env' = Environment.extend x s env in
    eval_ty env' m
