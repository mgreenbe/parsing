[%bs.raw {|require('./app.css')|}];

open ParseArith;

let ts = [INT(1), BINOP(Plus), INT(2), BINOP(Times), INT(3), BINOP(Minus), INT(4)];

let n = parse(ts);

let data = [|obj_of_node(parse(ts))|];

n |> string_of_node |> Js.log;

let component = ReasonReact.statelessComponent("App");

/* let child: Tree.dataT = [|{"name": "y", "children": [||]}|]; */
let make = (_children) => {
  ...component,
  render: (_self) =>
    <div
      style=(
        ReactDOMRe.Style.make(
          ~width="40rem",
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