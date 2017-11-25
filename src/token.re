type t =
  | INT(int)
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | POWER;

let string_of_token = (t) =>
  switch t {
  | INT(n) => string_of_int(n)
  | PLUS => "+"
  | MINUS => "-"
  | TIMES => "*"
  | DIVIDE => "/"
  | POWER => "^"
  };