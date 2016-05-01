%{
open Syntax
%}
%token SUCC PRED ISZERO IF THEN ELSE
%token NUMTY BOOLTY
%token LAMBDA MU
%token LPAREN RPAREN ARROW DOT COLON
%token EOL

%token <int> NUMV
%token <bool> BOOLV
%token <string> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel : Program { $1 }

Program :
  Expr EOL { Exp $1 }

Expr :
  NUMV        { Num $1 }
| BOOLV       { Bool $1 }
| ID          { Var $1 }
| SuccExpr    { $1 }
| PredExpr    { $1 }
| IsZeroExpr  { $1 }
| IfExpr      { $1 }
| Application { $1 }
| Abstraction { $1 }
| Fixpoint    { $1 }
| LPAREN Expr RPAREN { $2 }

SuccExpr :
  SUCC LPAREN Expr RPAREN { ExpSucc $3 }

PredExpr :
  PRED LPAREN Expr RPAREN { ExpPred $3 }

IsZeroExpr :
  ISZERO LPAREN Expr RPAREN { ExpIsZero $3 }

IfExpr :
  IF Expr THEN Expr ELSE Expr { ExpIf ($2, $4, $6) }

Application :
  LPAREN Expr Expr RPAREN { ExpApp ($2, $3) }

Abstraction :
  LPAREN LAMBDA ID COLON Type DOT Expr RPAREN { ExpAbs ($3, $5, $7) }

Fixpoint :
  LPAREN MU ID COLON Type DOT Expr RPAREN { ExpFix ($3, $5, $7) }

Type :
  NUMTY           { TyNum }
| BOOLTY          { TyBool }
| LPAREN Type ARROW Type RPAREN { TyFun ($2, $4) }
