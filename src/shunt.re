type binOp =
  | Plus
  | Minus
  | Times
  | Divide
  | Power;

let prec = (op) =>
  switch op {
  | Plus
  | Minus => 0
  | Times
  | Divide => 1
  | Power => 2
  };

let string_of_binOp = (op) =>
  switch op {
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | Divide => "/"
  | Power => "^"
  };

type token =
  | BINOP(binOp)
  | INT(int)
  | LPAREN
  | RPAREN;

let string_of_token = (t) =>
  switch t {
  | BINOP(op) => string_of_binOp(op)
  | INT(n) => string_of_int(n)
  | LPAREN => "("
  | RPAREN => ")"
  };

type node =
  | IntLitNode(int)
  | BinOpNode((binOp, (node, node)));

let rec obj_of_node = (node: node) : Tree.dataT =>
  switch node {
  | IntLitNode(n) => {"name": string_of_int(n), "children": [||]}
  | BinOpNode((op, (left, right))) => {
      "name": string_of_binOp(op),
      "children": [|obj_of_node(left), obj_of_node(right)|]
    }
  };

let rec string_of_node = (node) =>
  switch node {
  | IntLitNode(n) => string_of_int(n)
  | BinOpNode((op, (left, right))) =>
    "("
    ++ string_of_node(left)
    ++ " "
    ++ string_of_binOp(op)
    ++ " "
    ++ string_of_node(right)
    ++ ")"
  };

let binOpNode = (op, left, right) => BinOpNode((op, (left, right)));

let rec consumeBinOp = (o, exprs, punc) =>
  switch (exprs, punc) {
  | (_, [BINOP(p), ..._]) when o == Power && p == Power => (exprs, [BINOP(o), ...punc])
  | ([e, f, ...g], [BINOP(p), ...q]) when prec(o) <= prec(p) =>
    consumeBinOp(o, [binOpNode(p, f, e), ...g], q)
  /* Subtlety with right associativity in the above. */
  | (_, [])
  | ([_, _, ..._], [_, ..._]) => (exprs, [BINOP(o), ...punc])
  | ([], _)
  | ([_], _) => Js.Exn.raiseError("Expression stack too small.")
  };

let rec consumeRParen = (exprs, punc) =>
  switch (exprs, punc) {
  | ([e, f, ...g], [BINOP(p), ...q]) => consumeRParen([binOpNode(p, f, e), ...g], q)
  | (_, [LPAREN, ...q]) => (exprs, q)
  | _ => Js.Exn.raiseError("That shouldn't have happened.")
  };

let printExprs = (exprs) => {
  print_endline("Expression stack: ");
  exprs |> List.iter((e) => string_of_node(e) |> print_endline)
};

let printPunc = (punc) => {
  print_endline("Punctuation stack: ");
  punc |> List.iter((t) => string_of_token(t) |> print_endline)
};

let consume = ((exprs, punc), t) => {
  printExprs(exprs);
  printPunc(punc);
  switch t {
  | INT(n) => ([IntLitNode(n), ...exprs], punc)
  | LPAREN => (exprs, [LPAREN, ...punc])
  | RPAREN => consumeRParen(exprs, punc)
  | BINOP(o) => consumeBinOp(o, exprs, punc)
  }
};

let rec clearPunc = ((exprs, punc)) =>
  switch (exprs, punc) {
  | ([e], []) => e
  | ([e, f, ...g], [BINOP(p), ...q]) => clearPunc(([binOpNode(p, f, e), ...g], q))
  /* | ([], _) */
  /* | ([_], _) => Js.Exn.raiseError("Expression stack too small to clear punctuation stack.") */
  /* | ([_, ..._], []) => Js.Exn.raiseError("Leftover expressions.") */
  | _ => Js.Exn.raiseError("Couldn't clear punctuation stack.")
  };

let ts = [INT(1), BINOP(Plus), INT(2)];

ts |> List.fold_left(consume, ([], [])) |> clearPunc |> string_of_node |> print_endline;