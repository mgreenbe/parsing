[@bs.module "react-d3-tree"] external tree : ReasonReact.reactClass = "default";

/* type dataT = array({. "name": string, "children": array(dataT)}); */
type dataT = {. "name": string, "children": array(dataT)};

type translateT = {. "x": int, "y": int};

type textLayoutT = {. "textAnchor": string, "x": int, "y": int};

type shapePropsT = {. "width": int, "height": int, "x": int, "y": int};

type nodeSvgShapeT = {. "shape": string, "shapeProps": shapePropsT};

let make =
    (
      ~data: array(dataT),
      ~orientation: string,
      ~pathFunc: string,
      ~translate: translateT,
      ~textLayout: textLayoutT,
      ~nodeSvgShape: nodeSvgShapeT,
      children
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=tree,
    ~props={
      "data": data,
      "orientation": orientation,
      "pathFunc": pathFunc,
      "translate": translate,
      "textLayout": textLayout,
      "nodeSvgShape": nodeSvgShape
    },
    children
  );