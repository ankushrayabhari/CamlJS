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

    return NativeArray.from(
      {length: num},
      (x, i) => JSON.parse(JSON.stringify(v))
    );
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

    return NativeArray.from({length: num}, (x, i) => f(i));
  },
  make_matrix: dimx => dimy => e => {
    if (dimx < 0 || dimy < 0) {
      throw new Error("Invalid_argument")
    }

    return NativeArray.from({length: dimx}, (x, i) =>
      NativeArray.from({length: dimy}, JSON.parse(JSON.stringify(e)))
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
    for (let i = 0; i < arr.length; i++) {
      f(arr[i]);
    }
  },
  iteri: f => arr => {
    for (let i = 0; i < arr.length; i++) {
      f(i)(arr[i]);
    }
  },
  map: f => a => {
    return a.map(e => JSON.parse(JSON.stringify(f(e))));
  },
  mapi: f => a => {
    return a.map((e, idx) => JSON.parse(JSON.stringify(f(idx)(e))));
  },
  fold_left: f => a => arr => {
    return arr.reduce((acc, curr) => f(acc)(curr), a)
  },
  fold_right: f => arr => a => {
    return lst.reduceRight((acc, curr) => f(curr)(acc), a);
  },
  mem: el => arr => {
    for (let i = 0; i < arr.length; i++) {
      if (compare(el)(arr[i]) == 0) {
        return true;
      }
    }
    return false;
  },
  sort: f => arr => {
    return arr.map(e => JSON.parse(JSON.stringify(e)))
              .sort((el1, el2) => f(el1)(el2));
  },
  sort_uniq: f => arr => {
    return arr.map(e => JSON.parse(JSON.stringify(e)))
              .sort((el1, el2) => f(el1)(el2))
              .filter((el, idx, arr) => idx == 0 || f(el)(arr[idx - 1]) != 0);
  }
};
|}


let destructure = {|
  const {
    length,
    get,
    set,
    make,
    nth,
    init,
    make_matrix,
    append,
    concat,
    sub,
    to_list,
    of_list,
    iter,
    iteri,
    map,
    mapi,
    fold_left,
    fold_right,
    mem,
    sort,
    sort_uniq
  } = Array;
|}
