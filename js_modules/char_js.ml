let impl = {|
const Char = {
  code: a => a.charCodeAt(0),
  chr: a => String.fromCharCode(a),
  lowercase_ascii: a => a.toLowerCase(),
  uppercase_ascii: a => a.toUpperCase(),
  compare: a => b => Pervasives.compare(Char.code(a))(Char.code(b)),
  equal:  a => b => Char.code(a) == Char.code(b)
};
|}

let destructure = {|
const {
  code,
  chr,
  lowercase_ascii,
  uppercase_ascii,
  compare,
  equal
} = Char;
|}
