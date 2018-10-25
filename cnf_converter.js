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

// Step 1 - Get rid of all productions larger than 3
let remove_large_productions = () => {
  console.log(start_variable);
  console.log(last_variable);
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
console.log(productions);
