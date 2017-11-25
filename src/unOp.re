type t =
  | Prefix(Token.t, int)
  | Postfix(Token.t, int);

let plus = Prefix(Token.PLUS, 2);

let minus = Prefix(Token.MINUS, 2);

let token = (op) =>
  switch op {
  | Prefix(t, _)
  | Postfix(t, _) => t
  };

let prec = (op) =>
  switch op {
  | Prefix(_, p)
  | Postfix(_, p) => p
  };

let string_of_op = (op) =>
  switch op {
  | Prefix(token, _) => Token.string_of_token(token)
  | Postfix(token, _) => Token.string_of_token(token) ++ "u"
  };