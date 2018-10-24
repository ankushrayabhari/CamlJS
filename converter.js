var fs = require('fs');
var grammar = JSON.parse(fs.readFileSync('grammar_converted.json', 'utf8'));

let int_map = {};
for (let i = 0; i < grammar["tokens"].length; i++) {
  int_map[grammar["tokens"][i]] = i;
}

let variables = Object.keys(grammar["productions"]);
for (let x = 0; x < variables.length; x++) {
  int_map[variables[x]] = x + grammar["tokens"].length;
}

console.log("(*")
for (let key in int_map) {
  console.log(key + ": " + int_map[key]);
}
console.log("*)");
console.log("")

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
    token_to_varid_map[grammar["tokens"][i]].join(";") + "]");
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
  val += "];  (* " + (x + grammar["tokens"].length) + " *)";
  console.log(val);
}
console.log("]");
