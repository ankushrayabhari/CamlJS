let impl = {|
const List = {
  length: a => a.length,
  cons: a => b => b.concat([a]),
  hd: a => {if (a.length == 0) {throw new Error("hd")} else return a[a.length - 1]},
  tl: a => {if (a.length == 0) {throw new Error("tl")} else return a.slice(0, a.length - 1)},
  nth: a => n => {if (a.length <= n) {throw new Error("nth")} else if (n < 0) {throw new Error("List.nth")} else return a[a.length - 1 - n];},
  nth_opt: a => n => a.length <= n || n < 0 ? null : a[a.length - 1 - n],
  rev: a => a.slice().reverse(),
  append: a => b => b.concat(a),
  flatten: a => [].concat(...a),
  iter: f => a => a.reduceRight((acc, curr) => f(curr), null),
  iteri: f => a => a.reduceRight((acc, curr, idx) => f(a.length - 1 - idx)(curr), null),
  map: f => a => a.slice().reverse().map(f).reverse(),
  mapi: f => a => a.slice().reverse().map((curr, idx) => f(idx)(curr)).reverse(),
  fold_left: f => a => lst => lst.reduceRight((acc, curr) => f(acc)(curr), a),
  fold_right: f => lst => a => lst.reduce((acc, curr) => f(curr)(acc), a),
  mem: el => lst => {for (let i = 0; i < lst.length; i++) { if (compare(el)(lst[i]) == 0) {return true;}} return false;},
  memq: el => lst => lst.indexOf(el) >= 0,
  find: f => lst => {for (let i = 0; i < lst.length; i++) { if (f(el)) {return lst[i];}} throw new Error("Not found");},
  find_opt: f => lst => {for (let i = 0; i < lst.length; i++) { if (f(el)) {return lst[i];}} return null;},
  filter: f => lst => lst.filter(f),
  sort: f => lst => lst.slice().sort((el1, el2) => f(el1)(el2)),
  sort_uniq: f => lst => lst.slice().sort((el1, el2) => f(el1)(el2)).filter((el, idx, lst) => idx == 0 || f(el)(lst[idx - 1]) != 0)
};
|}


let destructure = {|
const {
  length,
  cons,
  hd,
  tl,
  nth,
  nth_opt,
  rev,
  append,
  flatten,
  iter,
  iteri,
  map,
  mapi,
  fold_left,
  fold_right,
  mem,
  memq,
  find,
  find_opt,
  filter,
  sort,
  sort_uniq
} = List;
|}
