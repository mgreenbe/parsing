type assocT =
  | Left
  | Right;

type t =
  | Infix(Token.t, int, assocT);

let plus = Infix(Token.PLUS, 1, Left);

let minus = Infix(Token.MINUS, 1, Left);

let times = Infix(Token.TIMES, 3, Left);

let divide = Infix(Token.DIVIDE, 3, Left);

let power = Infix(Token.POWER, 4, Left);

let token = (op) =>
  switch op {
  | Infix(t, _, _) => t
  };

let prec = (op) =>
  switch op {
  | Infix(_, p, _) => p
  };

let assoc = (op) =>
  switch op {
  | Infix(_, _, a) => a
  };

let string_of_op = (op) =>
  switch op {
  | Infix(token, _, _) => Token.string_of_token(token)
  };