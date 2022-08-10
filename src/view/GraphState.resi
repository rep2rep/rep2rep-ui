module GraphNode: {
  type nodeData =
    | Token(TokenData.t)
    | Constructor(ConstructorData.t)

  type t

  let create: (Gid.t, ~x: float, ~y: float, nodeData) => t
  let data: t => array<ReactD3Graph.Node.t<[#token | #constructor]>>
  let move: (t, ~x: float, ~y: float) => t
  let setData: (t, nodeData) => t

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
}

module Selection: {
  type t

  let empty: t
  let nodes: t => array<Gid.t>
  let links: t => array<Gid.t>
  let toReactD3Selection: t => ReactD3Graph.Graph.Selection.t
  let fromReactD3Selection: ReactD3Graph.Graph.Selection.t => t
}

type t

let duplicate: (t, Gid.Map.t<Gid.t>) => t
let empty: t
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

module Stable: {
  module V1: {
    type t = t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
