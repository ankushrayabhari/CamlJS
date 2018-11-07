let impl = {|
const Array = {
  length: a => a.length,
  get: a => b => {
    if (b < 0 || b >= a.length) {
      throw new Error ("Invalid_argument: index out of bounds")
    }
    return a[b];
  },
  set: a => b => c => {
    if (b < 0 || b >= a.length) {
      throw new Error ("Invalid_argument: index out of bounds")
    }
    a[b] = c;
  },
  make: num => v => {
    if (num < 0) {
      throw new Error("Invalid_argument")
    }

    return Array.from({length: num}, (x, i) => JSON.parse(JSON.stringify(v)));
  },
  nth: a => n => {
    if (a.length <= n) {
      throw new Error("nth")
    } else if (n < 0) {
      throw new Error("List.nth")
    } else return a[a.length - 1 - n];
  },
  init: num => f => {
    if (num < 0) {
      throw new Error("Invalid_argument")
    }

    return Array.from({length: num}, (x, i) => f(i));
  },
  make_matrix: dimx => dimy => e => {
    if (dimx < 0 || dimy < 0) {
      throw new Error("Invalid_argument")
    }

    return Array.from({length: dimx}, (x, i) =>
      Array.from({length: dimy}, JSON.parse(JSON.stringify(e)))
    );
  },
  append: a => b => {
    return a.concat(b).map(e => JSON.parse(JSON.stringify(e)));
  },
  concat: lst => {
    return [].concat(...lst).map(e => JSON.parse(JSON.stringify(e)));
  },
  sub: arr => start => len => {
    if (start < 0 || len < 0 || start + len > arr.length) {
      throw new Error("Invalid_argument");
    }

    let ret_arr = [];
    for (let i = start; i < start + len; i++) {
      ret_arr.push(JSON.parse(JSON.stringify(arr[i])));
    }
    return ret_arr;
  },
  to_list: arr => {
    let lst = arr.map(e => JSON.parse(JSON.stringify(e)));
    arr.reverse();
    return lst;
  },
  of_list: lst => {
    let arr = lst.map(e => JSON.parse(JSON.stringify(e)));
    arr.reverse();
    return arr;
  },
  iter: f => arr => {
    arr.map(f);
  },
  map: f => a => {
    return a.map(e => JSON.parse(JSON.stringify(f(e))));
  },
  mapi: f => a => {
    return a.map((e, idx) => JSON.parse(JSON.stringify(f(idx)(e))));
  },
  fold_left: f => a => lst => lst.reduceRight((acc, curr) => f(acc)(curr), a),
  fold_right: f => lst => a => lst.reduce((acc, curr) => f(curr)(acc), a),
  mem: el => lst => {
    for (let i = 0; i < lst.length; i++) {
      if (compare(el)(lst[i]) == 0) {
        return true;
      }
    }
    return false;
  },
  sort: f => lst => lst.slice().sort((el1, el2) => f(el1)(el2)).reverse(),
  sort_uniq: f => lst => {
    return lst.slice()
              .sort((el1, el2) => f(el1)(el2))
              .filter((el, idx, lst) => idx == 0 || f(el)(lst[idx - 1]) != 0)
              .reverse();
  }
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
