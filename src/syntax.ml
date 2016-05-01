exception Unprintable

type ty =
  TyNum
| TyBool
| TyFun of ty * ty

type exp =
  Num of int
| Bool of bool
| Var of string
| ExpSucc of exp
| ExpPred of exp
| ExpIsZero of exp
| ExpIf of exp * exp * exp
| ExpApp of exp * exp
| ExpAbs of string * ty * exp
| ExpFix of string * ty * exp

type value =
  VNum of int
| VBool of bool
| VAbs of string * ty * exp

type program =
  Exp of exp

let rec string_of_ty = function
  TyNum         -> "num"
| TyBool        -> "bool"
| TyFun (a, b)  ->
    Printf.sprintf "(%s->%s)" (string_of_ty a) (string_of_ty b)

let rec string_of_exp = function
  Num n -> string_of_int n
| Bool b -> string_of_bool b
| Var x -> x
| ExpSucc m -> "succ(" ^ (string_of_exp m) ^ ")"
| ExpPred m -> "pred(" ^ (string_of_exp m) ^ ")"
| ExpIsZero m -> "iszero(" ^ (string_of_exp m) ^ ")"
| ExpIf (l, m, n) ->
    Printf.sprintf "if %s then %s else %s"
      (string_of_exp l) (string_of_exp m) (string_of_exp n)
| ExpApp (m, n) ->
    Printf.sprintf "(%s %s)" (string_of_exp m) (string_of_exp n)
| ExpAbs (x, s, m) ->
    Printf.sprintf "(lambda %s:%s.%s)" x (string_of_ty s) (string_of_exp m)
| ExpFix (x, s, m) ->
    Printf.sprintf "(mu %s:%s.%s)" x (string_of_ty s) (string_of_exp m)

let rec string_of_value = function
  VNum n  -> string_of_int n
| VBool b -> string_of_bool b
| VAbs (x, s, m) ->
    Printf.sprintf "(lambda %s:%s.%s)" x (string_of_ty s) (string_of_exp m)
