type t =
  | INT(int)
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | POWER;

/* | LPAREN */
/* | RPAREN; */
let string_of_token = (t) =>
  switch t {
  | INT(n) => string_of_int(n)
  | PLUS => "+"
  | MINUS => "-"
  | TIMES => "*"
  | DIVIDE => "/"
  | POWER => "^"
  /* | LPAREN => "(" */
  /* | RPAREN => ")" */
  };