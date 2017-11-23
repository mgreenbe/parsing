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

let string_of_exprs = (exprs) =>
  exprs |> List.map(string_of_node) |> List.rev |> Array.of_list |> Js.Array.joinWith(", ");

let string_of_ops = (ops) =>
  ops |> List.map(string_of_token) |> List.rev |> Array.of_list |> Js.Array.joinWith(", ");

let rec consumeBinOp = (o, exprs, ops) =>
  switch (exprs, ops) {
  | (_, [BINOP(p), ..._]) when o == Power && p == Power => (exprs, [BINOP(o), ...ops])
  | ([e, f, ...g], [BINOP(p), ...q]) when prec(o) <= prec(p) =>
    Js.log("    Shunt " ++ string_of_binOp(o) ++ " aside.");
    Js.log("    " ++ string_of_ops(q) ++ " >> " ++ string_of_token(BINOP(p)));
    Js.log(
      "    " ++ string_of_exprs(g) ++ " >> " ++ string_of_node(f) ++ ", " ++ string_of_node(e)
    );
    Js.log("    " ++ string_of_exprs(g) ++ " << " ++ string_of_node(binOpNode(p, f, e)));
    consumeBinOp(o, [binOpNode(p, f, e), ...g], q)
  /* Subtlety with right associativity in the above. */
  | (_, [])
  | (_, [LPAREN, ..._])
  | ([_, _, ..._], [_, ..._]) =>
    Js.log("    " ++ string_of_ops(ops) ++ " << " ++ string_of_token(BINOP(o)));
    (exprs, [BINOP(o), ...ops])
  | ([], _)
  | ([_], _) => Js.Exn.raiseError("Expression stack too small.")
  };

let logPopAndApply = (e, f, g, p, q) => {
  Js.log("    " ++ string_of_ops(q) ++ " >> " ++ string_of_token(BINOP(p)));
  Js.log("    " ++ string_of_exprs(g) ++ " >> " ++ string_of_node(f) ++ ", " ++ string_of_node(e));
  Js.log("    " ++ string_of_exprs(g) ++ " << " ++ string_of_node(binOpNode(p, f, e)))
};

let logConsumeToken = (t) => Js.log("  Consume t = " ++ string_of_token(t) ++ ":");

let logPushNode = (exprs, node) =>
  Js.log("    " ++ string_of_exprs(exprs) ++ " << " ++ string_of_node(node));

let logPushToken = (exprs, token) =>
  Js.log("    " ++ string_of_exprs(exprs) ++ " << " ++ string_of_token(token));

let rec consumeRParen = (exprs, ops) =>
  switch (exprs, ops) {
  | ([e, f, ...g], [BINOP(p), ...q]) =>
    logPopAndApply(e, f, g, p, q);
    consumeRParen([binOpNode(p, f, e), ...g], q)
  | (_, [LPAREN, ...q]) =>
    Js.log("    " ++ string_of_ops(q) ++ " >> " ++ string_of_token(LPAREN));
    (exprs, q)
  | _ => Js.Exn.raiseError("That shouldn't have happened.")
  };

let consume = ((exprs, ops), t) => {
  logConsumeToken(t);
  switch t {
  | INT(n) =>
    logPushNode(exprs, IntLitNode(n));
    ([IntLitNode(n), ...exprs], ops)
  | LPAREN =>
    logPushToken(exprs, LPAREN);
    (exprs, [LPAREN, ...ops])
  | RPAREN => consumeRParen(exprs, ops)
  | BINOP(o) => consumeBinOp(o, exprs, ops)
  }
};

let rec clearOps = (exprs, ops) =>
  switch (exprs, ops) {
  | ([e], []) => e
  | ([e, f, ...g], [BINOP(p), ...q]) =>
    logPopAndApply(e, f, g, p, q);
    clearOps([binOpNode(p, f, e), ...g], q)
  | _ => Js.Exn.raiseError("Couldn't clear op stack.")
  };

let rec parse = (~closer=None, ~nodes=[], ~ops=[], ts) =>
  switch (ts, closer) {
  /* End of token stream */
  | ([], None) => clearOps(nodes, ops)
  | ([INT(n), ...moreTs], _) =>
    logPushNode(nodes, IntLitNode(n));
    parse(~closer, ~nodes=[IntLitNode(n), ...nodes], ~ops, moreTs)
  | ([BINOP(o), ..._], _) => consumeBinOp(o, nodes, ops)
  };
/*returns (e, leftoverTokens)*/