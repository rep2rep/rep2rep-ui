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
  let deleteLink = (t, link) => {
    ...t,
    graph: t.graph->GraphState.deleteLink(link),
  }
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