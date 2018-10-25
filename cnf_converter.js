let fs = require('fs');
let grammar = JSON.parse(fs.readFileSync('grammar.json', 'utf8'));

let tokens = grammar["tokens"];
let num_tokens = tokens.length;
let productions = grammar["productions"];
let variables = Object.keys(productions);
let num_variables = variables.length;
let start_variable = num_tokens;

// Set values in variable_to_id_map
let variable_to_id_map = new Map();
for (let i = 0; i < num_tokens; i++) {
  variable_to_id_map.set(tokens[i].name, i);
}
for (let i = 0; i < num_variables; i++) {
  variable_to_id_map.set(variables[i], i + num_tokens);
}

let get_variable_id = (name) => {
  return variable_to_id_map.get(name);
}

let is_unit_production = (production) => {
  return production.length == 1 && get_variable_id(production[0]) >= num_tokens;
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

// Step 1 - Get rid of all unit productions
let remove_unit_productions = (var_id) => {
  let to_add_productions = [];
  let to_remove = new Set();
  let productions = get_productions(var_id);


  for (let production of productions) {
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

remove_unit_productions(start_variable);
console.log(productions);
