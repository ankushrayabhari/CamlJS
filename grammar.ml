(*
Int _: 0
LowercaseIdent _: 1
Plus: 2
Minus: 3
Times: 4
Divide: 5
GreaterThan: 6
LessThan: 7
GreaterThanOrEqual: 8
LessThanOrEqual: 9
NotEqual: 10
Equal: 11
Negation: 12
FunctionArrow: 13
LParen: 14
RParen: 15
If: 16
Then: 17
Else: 18
Fun: 19
SemiColon: 20
Let: 21
Rec: 22
In: 23
Expr: 24
LetExpr: 25
LetRecDecl: 26
LetDecl: 27
LetEqual: 28
LetAssignExpr: 29
LetAssignInExpr: 30
AnonFuncExpr: 31
FunctionDecl: 32
FunctionBody: 33
OneOrMoreLowercaseIdent _: 34
SemiColonExpr: 35
SemiColonExprEnd: 36
IfExpr: 37
IfElseExprBody: 38
IfElseExprThen: 39
IfElseExprThenBody: 40
ElseExpr: 41
ComparisonExpr: 42
ComparisonExprEnd: 43
AddExpr: 44
AddExprEnd: 45
TimesExpr: 46
TimesExprEnd: 47
FunctionCallExpr: 48
ParenExpr: 49
ParenExprEnd: 50
PrefixExpr: 51
*)

let token_to_varid = Tokenizer.(function
    | Int _ -> [0;24]
    | LowercaseIdent _ -> [1;24;34]
    | Plus -> [2]
    | Minus -> [3]
    | Times -> [4]
    | Divide -> [5]
    | GreaterThan -> [6]
    | LessThan -> [7]
    | GreaterThanOrEqual -> [8]
    | LessThanOrEqual -> [9]
    | NotEqual -> [10]
    | Equal -> [11]
    | Negation -> [12]
    | FunctionArrow -> [13]
    | LParen -> [14]
    | RParen -> [15]
    | If -> [16]
    | Then -> [17]
    | Else -> [18]
    | Fun -> [19]
    | SemiColon -> [20]
    | Let -> [21]
    | Rec -> [22]
    | In -> [23]
)

let rules = [
  [(21,27);(21,26);(19,32);(37,36);(16,38);(42,43);(44,45);(46,47);(48,49);(14,50);(12,51);(12,24);];  (* 24 *)
  [(21,27);(21,26);(19,32);(37,36);(16,38);(42,43);(44,45);(46,47);(48,49);(14,50);(12,51);(12,24);];  (* 25 *)
  [(22,27);];  (* 26 *)
  [(1,28);];  (* 27 *)
  [(11,29);];  (* 28 *)
  [(25,30);];  (* 29 *)
  [(23,31);];  (* 30 *)
  [(19,32);(37,36);(16,38);(42,43);(44,45);(46,47);(48,49);(14,50);(12,51);(12,24);];  (* 31 *)
  [(34,33);];  (* 32 *)
  [(13,31);];  (* 33 *)
  [(34,1);];  (* 34 *)
  [(37,36);(16,38);(42,43);(44,45);(46,47);(48,49);(14,50);(12,51);(12,24);];  (* 35 *)
  [(20,35);];  (* 36 *)
  [(16,38);(42,43);(44,45);(46,47);(48,49);(14,50);(12,51);(12,24);];  (* 37 *)
  [(37,39);];  (* 38 *)
  [(17,37);(17,40);];  (* 39 *)
  [(37,40);];  (* 40 *)
  [(18,37);];  (* 41 *)
  [(42,43);(44,45);(46,47);(48,49);(14,50);(12,51);(12,24);];  (* 42 *)
  [(11,44);(7,44);(6,44);(9,44);(8,44);(10,44);];  (* 43 *)
  [(44,45);(46,47);(48,49);(14,50);(12,51);(12,24);];  (* 44 *)
  [(2,46);(3,46);];  (* 45 *)
  [(46,47);(48,49);(14,50);(12,51);(12,24);];  (* 46 *)
  [(4,48);(5,48);];  (* 47 *)
  [(48,49);(14,50);(12,51);(12,24);];  (* 48 *)
  [(14,50);(12,51);(12,24);];  (* 49 *)
  [(49,15);];  (* 50 *)
  [(12,51);(12,24);];  (* 51 *)
]
