type t =
  | Unary(UnOp.t)
  | Binary(BinOp.t);

let uPlus = Unary(UnOp.plus);

let uMinus = Unary(UnOp.minus);

let bPlus = Binary(BinOp.plus);

let bMinus = Binary(BinOp.minus);

let times = Binary(BinOp.times);

let divide = Binary(BinOp.divide);

let power = Binary(BinOp.power);

let token = (op) =>
  switch op {
  | Unary(u) => UnOp.token(u)
  | Binary(b) => BinOp.token(b)
  };

let prec = (op) =>
  switch op {
  | Unary(u) => UnOp.prec(u)
  | Binary(b) => BinOp.prec(b)
  };

let string_of_op = (op) =>
  switch op {
  | Unary(uop) => UnOp.string_of_op(uop)
  | Binary(bop) => BinOp.string_of_op(bop)
  };

let string_of_ops = (ops) =>
  ops |> List.map(string_of_op) |> List.rev |> Array.of_list |> Js.Array.joinWith(", ");