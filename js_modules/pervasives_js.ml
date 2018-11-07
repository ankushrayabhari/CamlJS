let impl = {|
const NativeString = global.String;
const NativeArray = global.Array;
const Pervasives = {
  min: a => b => Math.min(a, b),
  max: a => b => Math.max(a, b),
  compare: a => b => a < b ? -1 : a > b ? 1 : 0,
  not: a => !a,
  succ: x => x + 1,
  pred: x => x - 1,
  abs: Math.abs,
  max_int: Number.MAX_SAFE_INTEGER,
  min_int: Number.MIN_SAFE_INTEGER,
  sqrt: Math.sqrt,
  exp: Math.exp,
  log: Math.log,
  log10: Math.log10,
  expm1: Math.expm1,
  log1p: Math.log1p,
  cos: Math.cos,
  sin: Math.sin,
  tan: Math.tan,
  acos: Math.acos,
  asin: Math.asin,
  atan: Math.atan,
  atan2: a => b => Math.atan2(a, b),
  hypot: a => b => Math.hypot(a, b),
  cosh: Math.cosh,
  sinh: Math.sinh,
  tanh: Math.tanh,
  ceil: Math.ceil,
  floor: Math.floor,
  abs_float: Math.abs,
  mod_float: a => b => a % b,
  float_of_int: a => a,
  int_of_float: Math.trunc,
  truncate: Math.trunc,
  infinity: Number.POSITIVE_INFINITY,
  neg_infinity: Number.NEGATIVE_INFINITY,
  nan: Number.NaN,
  max_float: Number.MAX_VALUE,
  min_float: Number.MIN_VALUE,
  int_of_char: a => a.charCodeAt(0),
  char_of_int: NativeString.fromCharCode,
  ignore: a => undefined,
  string_of_bool: a => a.toString(),
  bool_of_string: a => {
    if (a === 'true') {
      return true;
    } else if (a === 'false') {
      return false;
    } else {
      throw new Error("invalid argument")
    }
  },
  bool_of_string_opt: a => a === 'true' ? true : a === 'false' ? false : null,
  string_of_int: a => (a).toString(),
  int_of_string: a => {
    let v = Number.parseInt(a);
    if (Number.isNaN(v)) {
      throw new Error("int_of_string")
    } else return v;
  },
  int_of_string_opt: a => {
    let v = Number.parseInt(a);
    return Number.isNaN(v) ? null : v;
  },
  string_of_float: a => (a).toString(),
  float_of_string: a => {
    let v = Number.parseFloat(a);
    if (Number.isNaN(v)) {
      throw new Error("float_of_string")
    } else return v;
  },
  float_of_string_opt: a => {
    let v = Number.parseFloat(a);
    return Number.isNaN(v) ? null : v
  },
  print_char: a => (process.stdout.write(a), undefined),
  print_string: a => (process.stdout.write(a), undefined),
  print_int: a => (process.stdout.write(string_of_int(a)), undefined),
  print_float: a => (process.stdout.write(string_of_float(a)), undefined),
  print_endline: a => console.log(a),
  print_newline: () => console.log("")
};
|}

let destructure = {|
const {
  min,
  max,
  compare,
  not,
  succ,
  pred,
  abs,
  max_int,
  min_int,
  sqrt,
  exp,
  log,
  log10,
  expm1,
  log1p,
  cos,
  sin,
  tan,
  acos,
  asin,
  atan,
  atan2,
  hypot,
  cosh,
  sinh,
  tanh,
  ceil,
  floor,
  abs_float,
  mod_float,
  float_of_int,
  int_of_float,
  truncate,
  infinity,
  neg_infinity,
  nan,
  max_float,
  min_float,
  int_of_char,
  char_of_int,
  ignore,
  string_of_bool,
  bool_of_string,
  bool_of_string_opt,
  string_of_int,
  int_of_string,
  int_of_string_opt,
  string_of_float,
  float_of_string,
  float_of_string_opt,
  print_char,
  print_string,
  print_int,
  print_float,
  print_endline,
  print_newline
} = Pervasives;
|}
