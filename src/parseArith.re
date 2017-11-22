type binOp =
  | Plus
  | Minus
  | Times
  | Divide
  | Power;

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
  | INT(int);

let string_of_token = (t) =>
  switch t {
  | BINOP(op) => string_of_binOp(op)
  | INT(n) => string_of_int(n)
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

let binOpNode = (~op, ~left, ~right) => BinOpNode((op, (left, right)));

let precedence = (op) =>
  switch op {
  | Plus
  | Minus => 0
  | Times
  | Divide => 1
  | Power => 2
  };

let string_of_leftTree = (tree) =>
  switch tree {
  | None => "None"
  | Some(tree) => string_of_node(tree)
  };

let string_of_prevOp = (op) =>
  switch op {
  | None => "None"
  | Some(op) => string_of_binOp(op)
  };

let string_of_tokens = (ts) =>
  ts |> List.map(string_of_token) |> List.fold_left((acc, s) => acc ++ " " ++ s, "");

let rec parse = (~leftTree=?, ~prevOp=?, tokens) => {
  Format.sprintf(
    "\nleftTree: %s, prevOp: %s, tokens: %s\n",
    string_of_leftTree(leftTree),
    string_of_prevOp(prevOp),
    string_of_tokens(tokens)
  )
  |> print_endline;
  switch (leftTree, prevOp, tokens) {
  | (_, _, []) => Js.Exn.raiseError("Empty token list.")
  | (_, _, [BINOP(_), ..._]) => Js.Exn.raiseError("Lonely binary operator.")
  | (None, None, [INT(n)]) => IntLitNode(n)
  | (None, None, [INT(n), BINOP(op), ...ts]) => parse(~leftTree=IntLitNode(n), ~prevOp=op, ts)
  | (Some(leftTree), Some(prevOp), [INT(n)]) =>
    binOpNode(~op=prevOp, ~left=leftTree, ~right=IntLitNode(n))
  | (_, Some(Power), [_, BINOP(Power), ..._]) =>
    Js.Exn.raiseError("Powers of powers must be parenthesized.")
  | (Some(leftTree), Some(prevOp), [INT(n), BINOP(op), ...ts]) =>
    precedence(op) <= precedence(prevOp) ?
      parse(
        ~leftTree=binOpNode(~op=prevOp, ~left=leftTree, ~right=IntLitNode(n)),
        ~prevOp=op,
        ts
      ) :
      binOpNode(~op=prevOp, ~left=leftTree, ~right=parse(~leftTree=IntLitNode(n), ~prevOp=op, ts))
  | _ => Js.Exn.raiseError("Parse error.")
  }
};