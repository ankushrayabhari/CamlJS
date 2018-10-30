let impl = {|
const String = {
  length: a => a.length,
  get: a => idx => {
    if (idx < 0 || idx >= a.length) {
      throw new Error("Invalid_argument");
    }
    return a[idx];
  },
  make: len => c => c.repeat(len),
  init: len => f => {
    let str = 'a'.repeat(len);
    for (let i = 0; i < len; i++) {
      str[i] = f(i);
    }
    return str;
  },
  sub: s => start => len => {
    if (start < 0 || start >= s.length || start + length > len) {
      throw new Error("Invalid_argument");
    }
    return s.substring(start, start + len);
  },
  concat: s => lst => lst.join(s),
  iter: f => str => {
    for (let i = 0; i < str.length; i++) {
      f(str[i]);
    }
  },
  iteri: f => str => {
    for (let i = 0; i < str.length; i++) {
      f(i)(str[i]);
    }
  },
  map: f => str => {
    let map = 'a'.repeat(str.length);
    for (let i = 0; i < len; i++) {
      map[i] = f(str[i]);
    }
    return map;
  },
  mapi: f => str => {
    let map = 'a'.repeat(str.length);
    for (let i = 0; i < len; i++) {
      map[i] = f(i)(str[i]);
    }
    return map;
  },
  trim: str => str.trim(),
  index: str => c => {
    let idx = str.indexOf(c);
    if (idx < 0) {
      throw new Error("Not_found");
    }
    return idx;
  },
  index_opt: str => c => {
    let idx = str.indexOf(c);
    if (idx < 0) {
      return null;
    }
    return idx;
  },
  rindex: str => c => {
    let idx = str.lastIndexOf(c);
    if (idx < 0) {
      throw new Error("Not_found");
    }
    return idx;
  },
  rindex_opt: str => c => {
    let idx = str.lastIndexOf(c);
    if (idx < 0) {
      return null;
    }
    return idx;
  },
  index_from: str => idx => c => {
    if (idx < 0 || idx >= str.length) {
      throw new Error("Invalid_argument");
    }
    let idx = str.indexOf(c, idx);
    if (idx < 0) {
      throw new Error("Not_found");
    }
    return idx;
  },
  index_from_opt: str => idx => c => {
    if (idx < 0 || idx >= str.length) {
      throw new Error("Invalid_argument");
    }
    let idx = str.indexOf(c, idx);
    if (idx < 0) {
      return null;
    }
    return idx;
  },
  rindex_from: str => idx => c => {
    if (idx + 1 < 0 || idx + 1 >= str.length) {
      throw new Error("Invalid_argument");
    }
    let idx = str.lastIndexOf(c, idx);
    if (idx < 0) {
      throw new Error("Not_found");
    }
    return idx;
  },
  rindex_from_opt: str => idx => c => {
    if (idx + 1 < 0 || idx + 1 >= str.length) {
      throw new Error("Invalid_argument");
    }
    let idx = str.lastIndexOf(c, idx);
    if (idx < 0) {
      return null;
    }
    return idx;
  },
  contains: str => c => str.indexOf(c) >= 0,
  contains_from: str => idx => c => {
    if (idx < 0 || idx >= str.length) {
      throw new Error("Invalid_argument");
    }

    return str.indexOf(c, idx) >= 0;
  },
  rcontains_from: str => idx => c => {
    if (idx + 1 < 0 || idx + 1 >= str.length) {
      throw new Error("Invalid_argument");
    }
    return str.lastIndexOf(c, idx) >= 0;
  },
  uppercase_ascii: str => str.toUpperCase(),
  lowercase_ascii: str => str.toLowerCase(),
  capitalize_ascii: str => {
    if (str.length > 0) {
      return str[0].toUpperCase() + str.substring(1);
    }
    return "";
  },
  uncapitalize_ascii: str => {
    if (str.length > 0) {
      return str[0].toLowerCase() + str.substring(1);
    }
    return "";
  }
};
|}

let destructure = {|
  const {
    length,
    get,
    make,
    init,
    sub,
    concat,
    iter,
    iteri,
    map,
    mapi,
    trim,
    index,
    index_opt,
    rindex,
    rindex_opt,
    index_from,
    index_from_opt,
    rindex_from,
    rindex_from_opt,
    contains,
    contains_from,
    rcontains_from,
    uppercase_ascii,
    lowercase_ascii,
    capitalize_ascii
  } = String;
|}
