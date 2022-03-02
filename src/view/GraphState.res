let toUuid = nodeId => nodeId->ReactD3Graph.Node.Id.toString->Uuid.fromString
let fromUuid = uuid => uuid->Uuid.toString->ReactD3Graph.Node.Id.ofString

module GraphNode = {
  type nodeData =
    | Token(TokenData.t)
    | Constructor(ConstructorData.t)

  type t = ReactD3Graph.Node.t<nodeData>

  let tokenConfig = _ => ReactD3Graph.Node.Config.create()
  let constructorConfig = _ =>
    ReactD3Graph.Node.Config.create(
      ~size={"width": 220., "height": 220.},
      ~viewGenerator=node => {
        <svg> <circle cx="6" cy="6" r="5" fill="white" stroke="black" /> </svg>
      },
      (),
    )

  let create = (id, ~x, ~y, payload) => {
    let config = switch payload {
    | Token(data) => tokenConfig(data)
    | Constructor(data) => constructorConfig(data)
    }
    ReactD3Graph.Node.create(~id=fromUuid(id), ~payload, ~x, ~y, ~config, ())
  }

  let data = t => [t]
  let id = t => ReactD3Graph.Node.id(t)->toUuid
  let duplicate = (t, newId) => t->ReactD3Graph.Node.setId(newId->fromUuid)
  let move = (t, ~x, ~y) => t->ReactD3Graph.Node.setX(x)->ReactD3Graph.Node.setY(y)
}

module GraphLink = {
  type t = ReactD3Graph.Link.t<int>

  let create = (id, ~source, ~target) =>
    ReactD3Graph.Link.create(
      ~source=fromUuid(source),
      ~target=fromUuid(target),
      ~id=Uuid.toString(id)->ReactD3Graph.Link.Id.ofString,
      ~payload=0,
      (),
    )

  let data = t => [t]
  let id = t =>
    ReactD3Graph.Link.id(t)->Option.getExn->ReactD3Graph.Link.Id.toString->Uuid.fromString
  let source = t => ReactD3Graph.Link.source(t)->toUuid
  let target = t => ReactD3Graph.Link.target(t)->toUuid
  let duplicate = (t, newSource, newTarget) =>
    t
    ->ReactD3Graph.Link.setSource(newSource->fromUuid)
    ->ReactD3Graph.Link.setTarget(newTarget->fromUuid)
}

module Selection = {
  type t = {
    nodes: array<Uuid.t>,
    links: array<Uuid.t>,
  }

  let empty = {
    nodes: [],
    links: [],
  }

  let nodes = t => t.nodes
  let links = t => t.links
  let toReactD3Selection = t => {
    ReactD3Graph.Graph.Selection.nodes: t.nodes->Array.map(fromUuid),
    links: t.links->Array.map(id => id->Uuid.toString->ReactD3Graph.Link.Id.ofString),
  }
  let fromReactD3Selection = selection => {
    nodes: selection.ReactD3Graph.Graph.Selection.nodes->Array.map(toUuid),
    links: selection.ReactD3Graph.Graph.Selection.links->Array.map(id =>
      id->ReactD3Graph.Link.Id.toString->Uuid.fromString
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
    let id = idMap->Uuid.Map.get(GraphNode.id(node))->Option.getExn
    node->GraphNode.duplicate(id)
  }),
  links: t.links->Array.map(link => {
    let source = idMap->Uuid.Map.get(GraphLink.source(link))->Option.getExn
    let target = idMap->Uuid.Map.get(GraphLink.target(link))->Option.getExn
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
