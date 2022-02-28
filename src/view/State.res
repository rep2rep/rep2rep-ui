module TokenData = {
  type t = string

  let duplicate = t => t
}

module ConstructorData = {
  type t = string

  let duplicate = t => t
}

module GraphState = {
  let toUuid = nodeId => nodeId->ReactD3Graph.Node.Id.toString->Uuid.fromString
  let fromUuid = uuid => uuid->Uuid.toString->ReactD3Graph.Node.Id.ofString

  module GraphNode = {
    type nodeData =
      | Token(TokenData.t)
      | Constructor(ConstructorData.t)

    type t = ReactD3Graph.Node.t<nodeData>

    let create = (id, ~x, ~y, payload) =>
      ReactD3Graph.Node.create(~id=fromUuid(id), ~payload, ~x, ~y, ())

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
  let setSelection = (t, selection) => {...t, selection: selection}
}

module Construction = {
  module Metadata = {
    type t = {name: string}

    let name = t => t.name
    let setName = (t, name) => {name: name}
    let create = name => {name: name}

    let duplicate = t => {name: t.name}
  }

  type t = {
    metadata: Metadata.t,
    tokenData: Uuid.Map.t<TokenData.t>,
    constructorData: Uuid.Map.t<ConstructorData.t>,
    graph: GraphState.t,
  }

  let graph = t => t.graph
  let metadata = t => t.metadata

  let create = name => {
    metadata: Metadata.create(name),
    tokenData: Uuid.Map.empty(),
    constructorData: Uuid.Map.empty(),
    graph: GraphState.empty,
  }

  let duplicate = t => {
    let idMap = Uuid.Map.merge(t.tokenData, t.constructorData, (_, _, _) => Some(Uuid.create()))
    let mapUpdate: (Uuid.Map.t<'a>, (Uuid.t, 'a) => (Uuid.t, 'b)) => Uuid.Map.t<'b> = (map, f) => {
      let newMap = ref(Uuid.Map.empty())
      map->Uuid.Map.forEach((id, value) => {
        let (id', value') = f(id, value)
        newMap := newMap.contents->Uuid.Map.set(id', value')
      })
      newMap.contents
    }
    {
      metadata: t.metadata->Metadata.duplicate,
      tokenData: t.tokenData->mapUpdate((id, td) => {
        let newId = idMap->Uuid.Map.get(id)->Option.getExn
        (newId, TokenData.duplicate(td))
      }),
      constructorData: t.constructorData->mapUpdate((id, cd) => {
        let newId = idMap->Uuid.Map.get(id)->Option.getExn
        (newId, ConstructorData.duplicate(cd))
      }),
      graph: t.graph->GraphState.duplicate(idMap),
    }
  }

  let rename = (t, name) => {...t, metadata: t.metadata->Metadata.setName(name)}
  let addToken = (t, id, ~x, ~y) => {
    let tokenData = "tok"
    let node = GraphState.GraphNode.create(id, ~x, ~y, GraphState.GraphNode.Token(tokenData))
    {
      ...t,
      tokenData: t.tokenData->Uuid.Map.set(id, tokenData),
      graph: t.graph->GraphState.addNode(node),
    }
  }
  let addConstructor = (t, id, ~x, ~y) => {
    let constructorData = "con"
    let node = GraphState.GraphNode.create(
      id,
      ~x,
      ~y,
      GraphState.GraphNode.Constructor(constructorData),
    )
    {
      ...t,
      constructorData: t.constructorData->Uuid.Map.set(id, constructorData),
      graph: t.graph->GraphState.addNode(node),
    }
  }
  let moveNode = (t, id, ~x, ~y) => {
    ...t,
    graph: t.graph->GraphState.updateNode(id, GraphState.GraphNode.move(_, ~x, ~y)),
  }
  let duplicateNode = (t, ~oldId, ~newId) => {
    t
  }
  let connect = (t, ~linkId, ~source, ~target) => {
    let link = GraphState.GraphLink.create(linkId, ~source, ~target)
    {...t, graph: t.graph->GraphState.addLink(link)}
  }
  let deleteNode = (t, id) => {
    ...t,
    // TODO: remove node data from everywhere
    graph: t.graph->GraphState.deleteNode(id),
  }
  let deleteLink = (t, link) => t
  let setSelection = (t, selection) => {...t, graph: t.graph->GraphState.setSelection(selection)}
}

type t = {
  focused: option<Uuid.t>,
  order: array<Uuid.t>,
  constructions: Uuid.Map.t<UndoRedo.t<Construction.t>>,
}

let empty = {
  focused: None,
  order: [],
  constructions: Uuid.Map.empty(),
}

let focused = t => t.focused
let constructions = t =>
  t.order->Array.keepMap(id =>
    t.constructions->Uuid.Map.get(id)->Option.map(c => (id, UndoRedo.state(c)))
  )
let construction = (t, id) => t.constructions->Uuid.Map.get(id)->Option.map(UndoRedo.state)

let load = () => None
let store = t => ()

let newConstruction = (t, id, name) => {
  let c = Construction.create(name)->UndoRedo.create
  {
    focused: Some(id),
    order: t.order->Array.concat([id]),
    constructions: t.constructions->Uuid.Map.set(id, c),
  }
}

let deleteConstruction = (t, id) => {
  let focused = t.focused->Option.flatMap(curr =>
    if curr === id {
      None
    } else {
      Some(curr)
    }
  )
  {
    focused: focused,
    order: t.order->Array.filter(id' => id' !== id),
    constructions: t.constructions->Uuid.Map.remove(id),
  }
}

let focusConstruction = (t, id) => {...t, focused: id}

let duplicateConstruction = (t, ~oldId, ~newId) => {
  t
  ->construction(oldId)
  ->Option.map(Construction.duplicate)
  ->Option.map(construction => {
    focused: Some(newId),
    order: t.order->Array.flatMap(id' =>
      if id' === oldId {
        [oldId, newId]
      } else {
        [oldId]
      }
    ),
    constructions: t.constructions->Uuid.Map.set(newId, UndoRedo.create(construction)),
  })
  ->Option.getWithDefault(t)
}

let reorderConstructions = (t, order) => {...t, order: order}

let importConstruction = (t, id, construction) => {
  let c = Construction.duplicate(construction)->UndoRedo.create
  {
    focused: Some(id),
    order: t.order->Array.concat([id]),
    constructions: t.constructions->Uuid.Map.set(id, c),
  }
}

let updateConstruction = (t, id, f) => {
  ...t,
  constructions: t.constructions->Uuid.Map.update(
    id,
    Option.map(_, ur_construction => {
      let c = UndoRedo.state(ur_construction)
      let c' = f(c)
      ur_construction->UndoRedo.step(c')
    }),
  ),
}

let undo = (t, id) => {
  ...t,
  constructions: t.constructions->Uuid.Map.update(id, Option.map(_, UndoRedo.undo)),
}
let redo = (t, id) => {
  ...t,
  constructions: t.constructions->Uuid.Map.update(id, Option.map(_, UndoRedo.redo)),
}

let canUndo = (t, id) =>
  t.constructions->Uuid.Map.get(id)->Option.map(UndoRedo.canUndo)->Option.getWithDefault(false)
let canRedo = (t, id) =>
  t.constructions->Uuid.Map.get(id)->Option.map(UndoRedo.canRedo)->Option.getWithDefault(false)
