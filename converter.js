let grammar = {
  "tokens": [
    "Int",
    "LowercaseIdent",
    "Plus",
    "Minus",
    "Times",
    "Divide",
    "GreaterThan",
    "LessThan",
    "GreaterThanOrEqual",
    "LessThanOrEqual",
    "NotEqual",
    "Equal",
    "Negation",
    "FunctionArrow",
    "LParen",
    "RParen",
    "If",
    "Then",
    "Else",
    "Fun",
    "SemiColon",
    "Let",
    "Rec",
    "In"
  ],
  "productions": {
    "Expr": [
      ["LowercaseIdent"],
      ["Int"],
      ["Let", "LetDecl"],
      ["Let", "LetRecDecl"],
      ["Fun", "FunctionDecl"],
      ["IfExpr", "SemiColonExprEnd"],
      ["If", "IfElseExprBody"],
      ["ComparisonExpr", "ComparisonExprEnd"],
      ["AddExpr", "AddExprEnd"],
      ["TimesExpr", "TimesExprEnd"],
      ["FunctionCallExpr", "ParenExpr"],
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "LetExpr": [
      ["Let", "LetDecl"],
      ["Let", "LetRecDecl"],
      ["Fun", "FunctionDecl"],
      ["IfExpr", "SemiColonExprEnd"],
      ["If", "IfElseExprBody"],
      ["ComparisonExpr", "ComparisonExprEnd"],
      ["AddExpr", "AddExprEnd"],
      ["TimesExpr", "TimesExprEnd"],
      ["FunctionCallExpr", "ParenExpr"],
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "LetRecDecl": [
      ["Rec", "LetDecl"]
    ],
    "LetDecl": [
      ["LowercaseIdent", "LetEqual"]
    ],
    "LetEqual": [
      ["Equal", "LetAssignExpr"]
    ],
    "LetAssignExpr": [
      ["LetExpr", "LetAssignInExpr"]
    ],
    "LetAssignInExpr": [
      ["in", "AnonFuncExpr"]
    ],
    "AnonFuncExpr": [
      ["Fun", "FunctionDecl"],
      ["IfExpr", "SemiColonExprEnd"],
      ["If", "IfElseExprBody"],
      ["ComparisonExpr", "ComparisonExprEnd"],
      ["AddExpr", "AddExprEnd"],
      ["TimesExpr", "TimesExprEnd"],
      ["FunctionCallExpr", "ParenExpr"],
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "FunctionDecl": [
      ["OneOrMoreLowercaseIdent", "FunctionBody"]
    ],
    "FunctionBody": [
      ["FunctionArrow", "AnonFuncExpr"]
    ],
    "OneOrMoreLowercaseIdent": [
      ["OneOrMoreLowercaseIdent", "LowercaseIdent"],
      ["LowercaseIdent"]
    ],
    "SemiColonExpr": [
      ["IfExpr", "SemiColonExprEnd"],
      ["If", "IfElseExprBody"],
      ["ComparisonExpr", "ComparisonExprEnd"],
      ["AddExpr", "AddExprEnd"],
      ["TimesExpr", "TimesExprEnd"],
      ["FunctionCallExpr", "ParenExpr"],
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "SemiColonExprEnd": [
      ["SemiColon", "SemiColonExpr"],
    ],
    "IfExpr": [
      ["If", "IfElseExprBody"],
      ["ComparisonExpr", "ComparisonExprEnd"],
      ["AddExpr", "AddExprEnd"],
      ["TimesExpr", "TimesExprEnd"],
      ["FunctionCallExpr", "ParenExpr"],
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "IfElseExprBody": [
      ["IfExpr", "IfElseExprThen"]
    ],
    "IfElseExprThen": [
      ["Then", "IfExpr"],
      ["Then", "IfElseExprThenBody"]
    ],
    "IfElseExprThenBody": [
      ["IfExpr", "IfElseExprThenBody"]
    ],
    "ElseExpr": [
      ["Else", "IfExpr"]
    ],
    "ComparisonExpr": [
      ["ComparisonExpr", "ComparisonExprEnd"],
      ["AddExpr", "AddExprEnd"],
      ["TimesExpr", "TimesExprEnd"],
      ["FunctionCallExpr", "ParenExpr"],
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "ComparisonExprEnd": [
      ["Equal", "AddExpr"],
      ["LessThan", "AddExpr"],
      ["GreaterThan", "AddExpr"],
      ["LessThanOrEqual", "AddExpr"],
      ["GreaterThanOrEqual", "AddExpr"],
      ["NotEqual", "AddExpr"],
    ],
    "AddExpr": [
      ["AddExpr", "AddExprEnd"],
      ["TimesExpr", "TimesExprEnd"],
      ["FunctionCallExpr", "ParenExpr"],
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "AddExprEnd": [
      ["Plus", "TimesExpr"],
      ["Minus", "TimesExpr"],
    ],
    "TimesExpr": [
      ["TimesExpr", "TimesExprEnd"],
      ["FunctionCallExpr", "ParenExpr"],
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "TimesExprEnd": [
      ["Times", "FunctionCallExpr"],
      ["Divide", "FunctionCallExpr"]
    ],
    "FunctionCallExpr": [
      ["FunctionCallExpr", "ParenExpr"],
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "ParenExpr": [
      ["LParen", "ParenExprEnd"],
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ],
    "ParenExprEnd": [
      ["ParenExpr", "RParen"],
    ],
    "PrefixExpr": [
      ["Negation", "PrefixExpr"],
      ["Negation", "Expr"]
    ]
  }
}

let int_map = {};
for (let i = 0; i < grammar["tokens"].length; i++) {
  int_map[grammar["tokens"][i]] = i;
}

let variables = Object.keys(grammar["productions"]);
for (let x = 0; x < variables.length; x++) {
  int_map[variables[x]] = x + grammar["tokens"].length;
}

for (let key in int_map) {
  console.log(key + ": " + int_map[key]);
}
console.log("");

let token_to_varid_map = {};
for (let i = 0; i < grammar["tokens"].length; i++) {
  token_to_varid_map[grammar["tokens"][i]] = [i];
}
for (let x = 0; x < variables.length; x++) {
  for (let i = 0; i < grammar["productions"][variables[x]].length; i++) {
    if (grammar["productions"][variables[x]][i].length == 1) {
      token_to_varid_map[grammar["productions"][variables[x]][i][0]].push(
        x + grammar["tokens"].length);
    }
  }
}

console.log("let token_to_varid = Tokenizer.(function");
for (let i = 0; i < grammar["tokens"].length; i++) {
  console.log("    | " +
    grammar["tokens"][i] + " -> [" +
    token_to_varid_map[grammar["tokens"][i]].toString() + "]");
}
console.log(")")

console.log("");
console.log("let rules = [");
for (let x = 0; x < variables.length; x++) {
  let val = "  [";
  for (let i = 0; i < grammar["productions"][variables[x]].length; i++) {
    if (grammar["productions"][variables[x]][i].length == 2) {
      val +=
        "(" + int_map[grammar["productions"][variables[x]][i][0]] + "," +
        int_map[grammar["productions"][variables[x]][i][1]] + ");"
    }
  }
  val += "  (* " + (x + grammar["tokens"].length) + " *)";
  console.log(val);
}
console.log("]");
