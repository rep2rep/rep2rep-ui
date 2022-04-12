let toGid = nodeId => nodeId->ReactD3Graph.Node.Id.toString->Gid.fromString
let fromGid = uuid => uuid->Gid.toString->ReactD3Graph.Node.Id.ofString

module GraphNode = {
  type nodeData =
    | Token(TokenData.t)
    | Constructor(ConstructorData.t)

  type t = ReactD3Graph.Node.t<[#token | #constructor]>

  let unselectedFill = "white"
  let selectedFill = "rgb(220, 220, 220)"
  let unselectedStrokeWidth = "1"
  let selectedStrokeWidth = "2"

  let tokenConfig = data => {
    let size = TokenData.size(data)
    ReactD3Graph.Node.Config.create(
      ~size={"width": size["width"] *. 10. +. 20., "height": size["height"] *. 10. +. 20.},
      ~viewGenerator=node => {
        <>
          <rect
            x={"1"}
            y={"1"}
            width={size["width"]->Float.toString}
            height={size["height"]->Float.toString}
            rx="7"
            ry="7"
            fill={if ReactD3Graph.Node.selected(node) {
              selectedFill
            } else {
              unselectedFill
            }}
            stroke="black"
            strokeWidth={if ReactD3Graph.Node.selected(node) {
              selectedStrokeWidth
            } else {
              unselectedStrokeWidth
            }}
          />
          <g x={"1"} y={"1"}> {TokenData.render(data)} </g>
        </>
      },
      (),
    )
  }
  let constructorConfig = (content: ConstructorData.t) =>
    ReactD3Graph.Node.Config.create(
      ~size={"height": 190., "width": 170. +. String.approximateEmWidth(content.label) *. 150.},
      ~viewGenerator=node => {
        <>
          <circle
            cx="6"
            cy="6"
            r="5"
            fill={if ReactD3Graph.Node.selected(node) {
              selectedFill
            } else {
              unselectedFill
            }}
            stroke="black"
            strokeWidth={if ReactD3Graph.Node.selected(node) {
              selectedStrokeWidth
            } else {
              unselectedStrokeWidth
            }}
          />
          <text x="15" y="15"> {React.string(content.label)} </text>
        </>
      },
      (),
    )

  let create = (id, ~x, ~y, payload) => {
    let (config, payload) = switch payload {
    | Token(data) => (tokenConfig(data), #token)
    | Constructor(data) => (constructorConfig(data), #constructor)
    }
    ReactD3Graph.Node.create(~id=fromGid(id), ~payload, ~x, ~y, ~config, ())
  }

  let data = t => [t]
  let id = t => ReactD3Graph.Node.id(t)->toGid
  let duplicate = (t, newId) => t->ReactD3Graph.Node.setId(newId->fromGid)
  let move = (t, ~x, ~y) => t->ReactD3Graph.Node.setX(x)->ReactD3Graph.Node.setY(y)
}

module GraphLink = {
  type t = ReactD3Graph.Link.t<int>

  let makeConfig = () => {
    let offset = (src, tgt, _) =>
      (src, tgt)
      ->Option.both
      ->Option.map(((src, tgt)) => {
        let (x1, y1) = (src["x"], src["y"])
        let (x2, y2) = (tgt["x"], tgt["y"])
        let size = src->ReactD3Graph.Core.readKeyExn("size")
        switch src->ReactD3Graph.Core.readKeyExn("payload") {
        | #constructor => {
            let (x, y) = (x2 -. x1, y2 -. y1)
            let l = Js.Math.hypot(x, y)
            let (dx, dy) = (x /. l, y /. l)
            {
              "dx": 6. *. dx -. size["width"] /. 20. +. 6.,
              "dy": 6. *. dy -. size["height"] /. 20. +. 6.,
            }
          }
        | #token => {
            let (dx0, dy0) = (x2 -. x1, y2 -. y1)
            let (dx, dy) = if Js.Math.abs_float(dx0 /. dy0) > size["width"] /. size["height"] {
              let dx = size["width"] /. 20. *. Js.Math.sign_float(dx0)
              let dy = dy0 *. dx /. dx0
              (dx, dy)
            } else {
              let dy = size["height"] /. 20. *. Js.Math.sign_float(dy0)
              let dx = dx0 *. dy /. dy0
              (dx, dy)
            }
            {"dx": dx, "dy": dy}
          }
        }
      })
      ->Option.getWithDefault({"dx": 0.0, "dy": 0.0})

    ReactD3Graph.Link.Config.create(
      ~offsetSource={
        (src, tgt, breakPoints) => offset(src, tgt, breakPoints)
      },
      ~offsetTarget={
        (src, tgt, breakPoints) => offset(tgt, src, breakPoints)
      },
      (),
    )
  }

  let create = (id, ~source, ~target) =>
    ReactD3Graph.Link.create(
      ~source=fromGid(source),
      ~target=fromGid(target),
      ~id=Gid.toString(id)->ReactD3Graph.Link.Id.ofString,
      ~payload=0,
      ~config=makeConfig(),
      (),
    )

  let data = t => [t]
  let id = t =>
    ReactD3Graph.Link.id(t)->Option.getExn->ReactD3Graph.Link.Id.toString->Gid.fromString
  let source = t => ReactD3Graph.Link.source(t)->toGid
  let target = t => ReactD3Graph.Link.target(t)->toGid
  let duplicate = (t, newSource, newTarget) =>
    t
    ->ReactD3Graph.Link.setSource(newSource->fromGid)
    ->ReactD3Graph.Link.setTarget(newTarget->fromGid)
}

module Selection = {
  type t = {
    nodes: array<Gid.t>,
    links: array<Gid.t>,
  }

  let empty = {
    nodes: [],
    links: [],
  }

  let nodes = t => t.nodes
  let links = t => t.links
  let toReactD3Selection = t => {
    ReactD3Graph.Graph.Selection.nodes: t.nodes->Array.map(fromGid),
    links: t.links->Array.map(id => id->Gid.toString->ReactD3Graph.Link.Id.ofString),
  }
  let fromReactD3Selection = selection => {
    nodes: selection.ReactD3Graph.Graph.Selection.nodes->Array.map(toGid),
    links: selection.ReactD3Graph.Graph.Selection.links->Array.map(id =>
      id->ReactD3Graph.Link.Id.toString->Gid.fromString
    ),
  }
}

type t = {
  nodes: array<GraphNode.t>,
  links: array<GraphLink.t>,
  selection: Selection.t,
}

let empty = {
  nodes: [],
  links: [],
  selection: Selection.empty,
}

let duplicate = (t, idMap) => {
  nodes: t.nodes->Array.map(node => {
    let id = idMap->Gid.Map.get(GraphNode.id(node))->Option.getExn
    node->GraphNode.duplicate(id)
  }),
  links: t.links->Array.map(link => {
    let source = idMap->Gid.Map.get(GraphLink.source(link))->Option.getExn
    let target = idMap->Gid.Map.get(GraphLink.target(link))->Option.getExn
    link->GraphLink.duplicate(source, target)
  }),
  selection: Selection.empty,
}

let data = t => {
  ReactD3Graph.Data.nodes: t.nodes->Array.flatMap(GraphNode.data),
  links: t.links->Array.flatMap(GraphLink.data),
}

let selection = t => t.selection
let addNode = (t, node) => {...t, nodes: t.nodes->Array.concat([node])}
let addLink = (t, link) => {...t, links: t.links->Array.concat([link])}
let updateNode = (t, nodeId, f) => {
  ...t,
  nodes: t.nodes->Array.map(node =>
    if GraphNode.id(node) === nodeId {
      f(node)
    } else {
      node
    }
  ),
}
let deleteNode = (t, nodeId) => {
  ...t,
  nodes: t.nodes->Array.filter(node => GraphNode.id(node) !== nodeId),
}
let deleteLink = (t, linkId) => {
  ...t,
  links: t.links->Array.filter(link => GraphLink.id(link) !== linkId),
}
let setSelection = (t, selection) => {...t, selection: selection}

let incidentLinks = (t, ~nodeId) => {
  let inLinks = []
  let outLinks = []
  t.links->Array.forEach(link => {
    if GraphLink.source(link) === nodeId {
      outLinks->Js.Array2.push(GraphLink.id(link))->ignore
    }
    if GraphLink.target(link) === nodeId {
      inLinks->Js.Array2.push(GraphLink.id(link))->ignore
    }
  })
  {"in": inLinks, "out": outLinks}
}
