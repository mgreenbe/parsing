let logApplyBinaryOp = (op, arg1, arg2, node, nodes) =>
  Format.sprintf(
    "Pop %s from operator stack.\nPop %s and %s from the node stack.\nPush %s onto expression stack.\nNodes: %s",
    BinOp.string_of_op(op),
    Nd.string_of_node(arg1),
    Nd.string_of_node(arg2),
    Nd.string_of_node(node),
    Nd.string_of_nodes(nodes)
  )
  |> print_endline;

let logApplyUnaryOp = (op, arg, node, nodes) =>
  Format.sprintf(
    "Pop %s from operator stack.\nPop %s from the node stack.\nPush %s onto expression stack.\nNodes: %s",
    UnOp.string_of_op(op),
    Nd.string_of_node(arg),
    Nd.string_of_node(node),
    Nd.string_of_nodes(nodes)
  )
  |> print_endline;

let applyBinOp = (nodes, op) =>
  switch nodes {
  | [arg1, arg2, ...moreNodes] =>
    let node = Nd.BinOp(op, arg2, arg1);
    let newNodes = [node, ...moreNodes];
    logApplyBinaryOp(op, arg2, arg1, node, newNodes);
    newNodes
  | []
  | [_] => Js.Exn.raiseError("Can't apply binary operation: not enough arguments.")
  };

let applyUnOp = (nodes, op) =>
  switch nodes {
  | [arg, ...moreNodes] =>
    let node = Nd.UnOp(op, arg);
    let newNodes = [node, ...moreNodes];
    logApplyUnaryOp(op, arg, node, newNodes);
    newNodes
  | [] => Js.Exn.raiseError("Can't apply binary operation: not enough arguments.")
  };

let applyOp = (nodes, op) =>
  switch op {
  | Op.Unary(u) => applyUnOp(nodes, u)
  | Op.Binary(b) => applyBinOp(nodes, b)
  };

let logPushOp = (o, newOps) =>
  Format.sprintf(
    "Push %s onto operator stack.\nOperators: %s",
    Op.string_of_op(o),
    Op.string_of_ops(newOps)
  )
  |> print_endline;

let logPushInt = (n, newNodes) =>
  Format.sprintf("Push %d onto node stack.\nNodes: %s", n, Nd.string_of_nodes(newNodes))
  |> print_endline;

let rec pushOrShuntOp = (nodes, ops, o) =>
  switch (ops, o) {
  | ([], _) =>
    let newOps = [o, ...ops];
    logPushOp(o, newOps);
    (nodes, newOps)
  | ([p, ..._], Op.Binary(b)) when o == p && BinOp.assoc(b) == Right => (nodes, [o, ...ops])
  | ([p, ...q], _) =>
    Op.prec(o) <= Op.prec(p) ?
      pushOrShuntOp(applyOp(nodes, p), q, o) :
      {
        let newOps = [o, ...ops];
        logPushOp(o, newOps);
        (nodes, newOps)
      }
  };

let rec purgeOpStack = (nodes, ops) =>
  switch ops {
  | [] => nodes
  | [op, ...moreOps] => purgeOpStack(applyOp(nodes, op), moreOps)
  };

let rec parse =
  Token.(
    (~nodes=[], ~ops=[], ts) =>
      switch ts {
      | [] => (purgeOpStack(nodes, ops), [])
      | [INT(n), ...moreTs] =>
        let newNodes = [Nd.Int(n), ...nodes];
        logPushInt(n, newNodes);
        parse(~nodes=[Nd.Int(n), ...nodes], ~ops, moreTs)
      | [PLUS, ...moreTs] =>
        let (newNodes, newOps) = pushOrShuntOp(nodes, ops, Op.bPlus);
        parse(~nodes=newNodes, ~ops=newOps, moreTs)
      | [MINUS, ...moreTs] =>
        let op =
          switch nodes {
          | [] => Op.uMinus
          | [_, ..._] => Op.bMinus
          };
        let (newNodes, newOps) = pushOrShuntOp(nodes, ops, op);
        parse(~nodes=newNodes, ~ops=newOps, moreTs)
      | [TIMES, ...moreTs] =>
        let (newNodes, newOps) = pushOrShuntOp(nodes, ops, Op.times);
        parse(~nodes=newNodes, ~ops=newOps, moreTs)
      | [DIVIDE, ...moreTs] =>
        let (newNodes, newOps) = pushOrShuntOp(nodes, ops, Op.divide);
        parse(~nodes=newNodes, ~ops=newOps, moreTs)
      | [POWER, ...moreTs] =>
        let (newNodes, newOps) = pushOrShuntOp(nodes, ops, Op.power);
        parse(~nodes=newNodes, ~ops=newOps, moreTs)
      }
  );

let ts = Token.[MINUS, INT(1), DIVIDE, INT(4), PLUS, INT(2), TIMES, INT(3), PLUS, INT(5)];

let (n, _) = parse(ts);

Nd.string_of_nodes(n) |> print_endline;

let obj = Nd.obj_of_node(n |> List.hd);