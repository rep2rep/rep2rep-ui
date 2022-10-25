module GraphNode: {
  type nodeData =
    | Token(TokenData.t)
    | Constructor(ConstructorData.t)

  type t

  let create: (Gid.t, ~x: float, ~y: float, nodeData) => t
  let data: t => array<ReactD3Graph.Node.t<[#token | #constructor]>>
  let move: (t, ~x: float, ~y: float) => t
  let setData: (t, nodeData) => t
  let id: t => Gid.t
  let kind: t => [#token | #constructor]

  module Stable: {
    module V1: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

module GraphLink: {
  type t

  let create: (Gid.t, ~source: Gid.t, ~target: Gid.t, ~edgeData: EdgeData.t) => t
  let data: t => array<ReactD3Graph.Link.t<unit>>
  let setData: (t, EdgeData.t) => t
  let id: t => Gid.t
}

module Selection: {
  type t

  let hash: t => Hash.t

  let empty: t
  let nodes: t => array<Gid.t>
  let links: t => array<Gid.t>
  let ofNodes: array<Gid.t> => t
  let toReactD3Selection: t => ReactD3Graph.Graph.Selection.t
  let fromReactD3Selection: ReactD3Graph.Graph.Selection.t => t
}

type t

let hash: t => Hash.t
let isValid: t => Result.t<unit, array<string>>

let empty: t
let layout: (
  ~tokens: Gid.Map.t<TokenData.t>,
  ~constructors: Gid.Map.t<ConstructorData.t>,
  ~edges: Gid.Map.t<EdgeData.t>,
) => t

let duplicate: (t, Gid.Map.t<Gid.t>) => t
let data: t => ReactD3Graph.Data.t<[#token | #constructor], unit>
let selection: t => Selection.t

let addNode: (t, GraphNode.t) => t
let addLink: (t, GraphLink.t) => t
let updateNode: (t, Gid.t, GraphNode.t => GraphNode.t) => t
let updateLink: (t, Gid.t, GraphLink.t => GraphLink.t) => t
let deleteNode: (t, Gid.t) => t
let deleteLink: (t, Gid.t) => t
let setSelection: (t, Selection.t) => t
let incidentLinks: (t, ~nodeId: Gid.t) => {"in": array<Gid.t>, "out": array<Gid.t>}
let getNode: (t, Gid.t) => option<GraphNode.t>
let getLink: (t, Gid.t) => option<GraphLink.t>
let nodes: t => array<GraphNode.t>
let links: t => array<GraphLink.t>

module Stable: {
  module V1: {
    type t = t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
