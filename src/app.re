[%bs.raw {|require('./app.css')|}];

open Shunt;

/* let ts = [INT(1), BINOP(Plus), INT(2), BINOP(Times), INT(3), BINOP(Minus), INT(4)]; */
let ts = [
  INT(0),
  BINOP(Minus),
  LPAREN,
  INT(1),
  BINOP(Plus),
  INT(2),
  RPAREN,
  BINOP(Power),
  INT(3),
  BINOP(Power),
  LPAREN,
  INT(4),
  BINOP(Divide),
  INT(5),
  RPAREN
];

/* let obj = ts |> List.fold_left(consume, ([], [])) |> Shunt.clearOps |> obj_of_node; */
let obj = Parse.obj;

let data = [|obj|];

let component = ReasonReact.statelessComponent("App");

/* let child: Tree.dataT = [|{"name": "y", "children": [||]}|]; */
let make = (_children) => {
  ...component,
  render: (_self) =>
    <div
      style=(
        ReactDOMRe.Style.make(
          ~width="60rem",
          ~height="40rem",
          ~margin="2rem",
          ~backgroundColor="papayawhip",
          ()
        )
      )>
      <Tree
        data
        orientation="vertical"
        pathFunc="straight"
        translate={"x": 320, "y": 30}
        textLayout={"textAnchor": "start", "x": (-5), "y": 10}
        nodeSvgShape={
          "shape": "rect",
          "shapeProps": {"width": 20, "height": 20, "x": (-10), "y": 0}
        }
      />
    </div>
};