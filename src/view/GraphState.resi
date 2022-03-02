module GraphNode: {
  type nodeData =
    | Token(TokenData.t)
    | Constructor(ConstructorData.t)

  type t

  let create: (Uuid.t, ~x: float, ~y: float, nodeData) => t
  let data: t => array<ReactD3Graph.Node.t<nodeData>>
  let move: (t, ~x: float, ~y: float) => t
}

module GraphLink: {
  type t

  let create: (Uuid.t, ~source: Uuid.t, ~target: Uuid.t) => t
  let data: t => array<ReactD3Graph.Link.t<int>>
}

module Selection: {
  type t

  let empty: t
  let nodes: t => array<Uuid.t>
  let links: t => array<Uuid.t>
  let toReactD3Selection: t => ReactD3Graph.Graph.Selection.t
  let fromReactD3Selection: ReactD3Graph.Graph.Selection.t => t
}

type t

let duplicate: (t, Uuid.Map.t<Uuid.t>) => t
let empty: t
let data: t => ReactD3Graph.Data.t<GraphNode.nodeData, int>
let selection: t => Selection.t

let addNode: (t, GraphNode.t) => t
let addLink: (t, GraphLink.t) => t
let updateNode: (t, Uuid.t, GraphNode.t => GraphNode.t) => t
let deleteNode: (t, Uuid.t) => t
let deleteLink: (t, Uuid.t) => t
let setSelection: (t, Selection.t) => t
let incidentLinks: (t, ~nodeId: Uuid.t) => {"in": array<Uuid.t>, "out": array<Uuid.t>}
