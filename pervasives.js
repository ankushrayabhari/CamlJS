const min = a => b => Math.min(a, b)
const max = a => b => Math.max(a, b)
const compare = a => b => a < b ? -1 : a > b ? 1 : 0
const not = a => !a
const succ = x => x + 1
const pred = x => x - 1
const abs = Math.abs
const max_int = Number.MAX_SAFE_INTEGER
const min_int = Number.MIN_SAFE_INTEGER
const sqrt = Math.sqrt
const exp = Math.exp
const log = Math.log
const log10 = Math.log10
const expm1 = Math.expm1
const log1p = Math.log1p
const cos = Math.cos
const sin = Math.sin
const tan = Math.tan
const acos = Math.acos
const asin = Math.asin
const atan = Math.atan
const atan2 = a => b => Math.atan2(a, b)
const hypot = a => b => Math.hypot(a, b)
const cosh = Math.cosh
const sinh = Math.sinh
const tanh = Math.tanh
const ceil = Math.ceil
const floor = Math.floor
const abs_float = Math.abs
const mod_float = a => b => a % b
const float_of_int = a => a
const int_of_float = Math.trunc
const truncate = int_of_float
const infinity = Number.POSITIVE_INFINITY
const neg_infinity = Number.NEGATIVE_INFINITY
const nan = Number.NaN
const max_float = Number.MAX_VALUE
const min_float = Number.MIN_VALUE
const int_of_char = a => a.charCodeAt(0)
const char_of_int = String.fromCharCode
const ignore = a => undefined
const string_of_bool = a => a.toString()
const bool_of_string = a => a === 'true' ? true : a === 'false' ? false
const bool_of_string_opt = a => a === 'true' ? true : a === 'false' ? false : null
const string_of_int = a => (a).toString()
const int_of_string = a => { let v = Number.parseInt(a); return Number.isNaN(v) ? (throw new Error("int_of_string")) : v}
const int_of_string_opt = a => { let v = Number.parseInt(a); return Number.isNaN(v) ? null : v}
const string_of_float = a => (a).toString()
const float_of_string = a => { let v = Number.parseFloat(a); return Number.isNaN(v) ? (throw new Error("float_of_string")) : v}
const float_of_string_opt = a => { let v = Number.parseFloat(a); return Number.isNaN(v) ? null : v}
const print_char = a => process.stdout.write(a)
const print_string = a => process.stdout.write(a)
const print_int = a => process.stdout.write(a)
const print_float = a => process.stdout.write(a)
const print_endline = a => console.log(a)
const print_newline = () => console.log("")
