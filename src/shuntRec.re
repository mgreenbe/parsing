type leftOrRight =
  | Left
  | Right;

type infixOp =
  | BinPlus
  | BinMinus
  | Times
  | Divide
  | Power;

type prefixOp =
  | UnPlus
  | UnMinus;

type op =
  | InfixOp(infixOp)
  | PrefixOp(prefixOp);

type group =
  | PROG
  | PAREN;

type opToken =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | POWER;

type token =
  | INT(int)
  | BEGIN(group)
  | END(group)
  | OP(opToken);

type node =
  | IntNode(int)
  | InfixOpNode((infixOp, (node, node)))
  | PrefixOpNode((prefixOp, node));

let prec = (op) =>
  switch op {
  | InfixOp(BinPlus) => 1
  | InfixOp(BinMinus) => 1
  | PrefixOp(UnPlus) => 2
  | PrefixOp(UnMinus) => 2
  | InfixOp(Times) => 3
  | InfixOp(Divide) => 3
  | InfixOp(Power) => 4
  };

let assoc = (op) =>
  switch op {
  | BinPlus => Left
  | BinMinus => Left
  | Times => Left
  | Divide => Left
  | Power => Right
  };

let applyInfixOp = (nodes, op) =>
  switch nodes {
  | [left, right, ...moreNodes] => [InfixOpNode((op, (left, right))), ...moreNodes]
  | []
  | [_] => Js.Exn.raiseError("Can't apply binary operation: not enough arguments.")
  };

let applyPrefixOp = (nodes, op) =>
  switch nodes {
  | [arg, ...moreNodes] => [PrefixOpNode((op, arg)), ...moreNodes]
  | [] => Js.Exn.raiseError("Can't apply unary operation: not enough arguments.")
  };

let applyOp = (nodes, op) =>
  switch (nodes, op) {
  | ([m, n, ...moreNodes], InfixOp(p)) => [InfixOpNode((p, (m, n))), ...moreNodes]
  | ([n, ...moreNodes], PrefixOp(p)) => [PrefixOpNode((p, n)), ...moreNodes]
  | ([], InfixOp(_))
  | ([_], InfixOp(_)) => Js.Exn.raiseError("Can't apply binary operation: not enough arguments.")
  | ([], PrefixOp(_)) => Js.Exn.raiseError("Can't apply unary operation: not enough arguments.")
  };

let pushOrShuntOp = (nodes, ops, o) =>
  switch (ops, o) {
  | ([], _) => (nodes, [o, ...ops])
  | ([p, ..._], InfixOp(r)) when o == p && assoc(r) == Right => (nodes, [o, ...ops])
  | ([p, ...q], _) => prec(o) <= prec(p) ? (applyOp(nodes, p), q) : (nodes, [o, ...ops])
  };

let rec purgeOpStack = (nodes, ops) =>
  switch ops {
  | [] => nodes
  | [InfixOp(op), ...moreOps] => purgeOpStack(applyInfixOp(nodes, op), moreOps)
  | [PrefixOp(op), ...moreOps] => purgeOpStack(applyPrefixOp(nodes, op), moreOps)
  };

let disambiguate = (nodes, op) =>
  switch (nodes, op) {
  | ([], PLUS) => PrefixOp(UnPlus)
  | ([_, ..._], PLUS) => InfixOp(BinPlus)
  | ([], MINUS) => PrefixOp(UnMinus)
  | ([_, ..._], MINUS) => InfixOp(BinMinus)
  | (_, TIMES) => InfixOp(Times)
  | (_, DIVIDE) => InfixOp(Divide)
  | (_, POWER) => InfixOp(Power)
  };

let rec parse = (~nodes=[], ~ops=[], ts) =>
  switch ts {
  | [END(_), ...moreTs] => (purgeOpStack(nodes, ops), moreTs)
  | [INT(n), ...moreTs] => parse(~nodes=[IntNode(n), ...nodes], ~ops, moreTs)
  | [OP(op), ...moreTs] =>
    let (newNodes, newOps) = pushOrShuntOp(nodes, ops, disambiguate(nodes, op));
    parse(~nodes=newNodes, ~ops=newOps, moreTs)
  | [BEGIN(_), ...moreTs] =>
    let (groupNodes, leftoverTs) = parse(moreTs);
    parse(~nodes=List.append(groupNodes, nodes), ~ops, leftoverTs)
  | [] => Js.Exn.raiseError("Empty token stream.")
  };