let fs = require('fs');
let grammar = JSON.parse(fs.readFileSync('grammar.json', 'utf8'));

let tokens = grammar["tokens"];
let num_tokens = tokens.length;
let productions = grammar["productions"];
let variables = Object.keys(productions);
let last_variable = variables.length - 1 + num_tokens;
let start_variable = num_tokens;
let num_variables = variables.length + num_tokens;

// Set values in variable_to_id_map
let variable_to_id_map = new Map();
for (let i = 0; i < num_tokens; i++) {
  variable_to_id_map.set(tokens[i].name, i);
}
for (let i = 0; i < num_variables; i++) {
  variable_to_id_map.set(variables[i], i + num_tokens);
}

// Helper functions for accessing production information
let get_variable_id = (name) => {
  return variable_to_id_map.get(name);
}

let is_unit_production = (production) => {
  return production.length == 1 &&
         get_variable_id(production[0]) >= num_tokens;
}

let get_productions = (var_id) => {
  return (var_id < num_tokens) ? [] :
         productions[variables[var_id - num_tokens]];
}

let set_productions = (var_id, new_prod) => {
  let stringified_productions = new_prod.map(el => JSON.stringify(el));
  let no_duplicates_stringified_productions = stringified_productions.filter(
    (item, pos) => stringified_productions.indexOf(item) == pos);
  let no_duplicates_productions =
    no_duplicates_stringified_productions.map(JSON.parse);
  productions[variables[var_id - num_tokens]] = no_duplicates_productions;
}

let add_variable = (var_1, var_2) => {
  let name = var_1 + "_" + var_2 + "_" + num_variables;
  productions[name] = [
    [var_1, var_2]
  ];
  variables = Object.keys(productions);
  variable_to_id_map.set(name, num_variables);
  num_variables++;
  return name;
}

let is_large_production = (production) => {
  return production.length > 2;
}

let is_token_production = (production) => {
  return production.length == 1 && get_variable_id(production[0]) < num_tokens;
}

// Step 1 - Get rid of all productions larger than 3
let remove_large_productions = () => {
  for (let var_id = start_variable; var_id <= last_variable; var_id++) {
    for (let production of get_productions(var_id)) {
      while (is_large_production(production)) {
        let last_el = production.pop();
        let second_last_el = production.pop();
        let new_variable = add_variable(last_el, second_last_el);
        production.push(new_variable);
      }
    }
  }
}

// Step 2 - Get rid of all unit productions
let remove_unit_productions = (var_id) => {
  let to_add_productions = [];
  let to_remove = new Set();

  for (let production of get_productions(var_id)) {
    if (is_unit_production(production)) {
      let sub_variable = get_variable_id(production[0]);
      to_remove.add(production[0]);

      remove_unit_productions(sub_variable);
      for (let sub_production of get_productions(sub_variable)) {
        to_add_productions.push(sub_production);
      }
    }
  }

  set_productions(var_id,
    get_productions(var_id).concat(to_add_productions).filter(el =>
      !(el.length == 1 && to_remove.has(el[0]))
    )
  );
}

remove_large_productions();
remove_unit_productions(start_variable);

// Step 3 - the token_to_varid function
let token_to_varid_fn = () => {
  let tokenToVarId = new Map();
  for (let var_id = 0; var_id < num_tokens; var_id++) {
    tokenToVarId.set(tokens[var_id].name, [var_id]);
  }

  for (let var_id = start_variable; var_id <= last_variable; var_id++) {
    for (let production of get_productions(var_id)) {
      if (is_token_production(production)) {
        let val = tokenToVarId.get(production[0]);
        val.push(var_id);
        tokenToVarId.set(production[0], val);
      }
    }
  }

  let match_cases = "";
  for (let token of tokens) {
    match_cases += `  | ${token.name} ${token.parameter ? "_ " : ""}-> ` +
                   `[${tokenToVarId.get(token.name).join(";")}]\n`;
  }

  return `let token_to_varid = Tokenizer.(function\n${match_cases})\n`
}

// Step 4 - the rules list
let rules_lst = () => {
  let rules_lst = "";
  for (let var_id = start_variable; var_id < num_variables; var_id++) {
    let string_productions =
      get_productions(var_id)
      .filter(production => !is_token_production(production))
      .map((production) => `(${production.map(get_variable_id).join(",")})`)
      .join(";");
    rules_lst += `\n  [${string_productions}];`;
  }
  return `let rules = [${rules_lst}\n]\n`;
}

let start_variable_int = () => {
  return `let start_variable = ${start_variable}\n`;
}

let num_tokens_int = () => {
  return `let num_tokens = ${num_tokens}\n`;
}

let num_variables_int = () => {
  return `let num_variables = ${num_variables}\n`;
}

let auto_generated_variable = () => {
  return `let auto_generated_variable var = var > ${last_variable}\n`;
}

console.log(token_to_varid_fn());
console.log(rules_lst());
console.log(start_variable_int());
console.log(num_tokens_int());
console.log(num_variables_int());
console.log(auto_generated_variable());
