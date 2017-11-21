[%bs.raw {|require('./app.css')|}];

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
        data=[|
          {
            "name": "x",
            "children": [|{"name": "y", "children": [||]}, {"name": "z", "children": [||]}|]
          }
        |]
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