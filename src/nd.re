type t =
  | Int(int)
  | UnOp(UnOp.t, t)
  | BinOp(BinOp.t, t, t);

let rec obj_of_node = (node: t) : Tree.dataT =>
  switch node {
  | Int(n) => {"name": string_of_int(n), "children": [||]}
  | UnOp(op, arg) => {"name": UnOp.string_of_op(op), "children": [|obj_of_node(arg)|]}
  | BinOp(op, left, right) => {
      "name": BinOp.string_of_op(op),
      "children": [|obj_of_node(left), obj_of_node(right)|]
    }
  };

let rec string_of_node = (~depth=0, node) =>
  switch node {
  | Int(n) => string_of_int(n)
  | UnOp(op, arg) =>
    switch op {
    | UnOp.Prefix(_, _) => UnOp.string_of_op(op) ++ string_of_node(arg)
    | UnOp.Postfix(_, _) => string_of_node(arg) ++ UnOp.string_of_op(op)
    }
  | BinOp(op, arg1, arg2) =>
    let newDepth = depth + 1;
    switch (op, depth) {
    | (BinOp.Infix(_, _, _), 0) =>
      Format.sprintf(
        "%s %s %s",
        string_of_node(~depth=newDepth, arg1),
        BinOp.string_of_op(op),
        string_of_node(~depth=newDepth, arg2)
      )
    | (BinOp.Infix(_, _, _), _) =>
      Format.sprintf(
        "(%s %s %s)",
        string_of_node(~depth=newDepth, arg1),
        BinOp.string_of_op(op),
        string_of_node(~depth=newDepth, arg2)
      )
    /* "("
       ++ string_of_node(arg1)
       ++ " "
       ++ BinOp.string_of_op(op)
       ++ " "
       ++ string_of_node(arg2)
       ++ ")" */
    }
  };

let string_of_nodes = (nodes) =>
  nodes |> List.map(string_of_node) |> List.rev |> Array.of_list |> Js.Array.joinWith(", ");