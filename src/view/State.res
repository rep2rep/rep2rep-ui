module Construction = {
  module Metadata = {
    type t = {
      name: string,
      notes: string,
    }

    let name = t => t.name
    let notes = t => t.notes
    let setName = (t, name) => {...t, name: name}
    let setNotes = (t, notes) => {...t, notes: notes}
    let create = name => {name: name, notes: ""}

    let duplicate = t => {name: t.name, notes: t.notes}
  }

  type t = {
    metadata: Metadata.t,
    space: option<CSpace.conSpec>,
    tokenData: Gid.Map.t<TokenData.t>,
    constructorData: Gid.Map.t<ConstructorData.t>,
    edgeData: Gid.Map.t<EdgeData.t>,
    graph: GraphState.t,
  }

  let graph = t => t.graph
  let metadata = t => t.metadata
  let space = t => t.space

  let create = name => {
    metadata: Metadata.create(name),
    space: None,
    tokenData: Gid.Map.empty(),
    constructorData: Gid.Map.empty(),
    edgeData: Gid.Map.empty(),
    graph: GraphState.empty,
  }

  let isEmpty = t =>
    t.tokenData->Gid.Map.size === 0 &&
    t.constructorData->Gid.Map.size === 0 &&
    t.edgeData->Gid.Map.size === 0

  let setSpace = (t, space) => {...t, space: space}

  let mapUpdate = (map, f) => {
    let newMap = ref(Gid.Map.empty())
    map->Gid.Map.forEach((id, value) => {
      let (id', value') = f(id, value)
      newMap := newMap.contents->Gid.Map.set(id', value')
    })
    newMap.contents
  }

  let duplicate = t => {
    let idMap = Gid.Map.merge(t.tokenData, t.constructorData, (_, _, _) => Some(Gid.create()))
    {
      metadata: t.metadata->Metadata.duplicate,
      space: t.space,
      tokenData: t.tokenData->mapUpdate((id, td) => {
        let newId = idMap->Gid.Map.get(id)->Option.getExn
        (newId, TokenData.duplicate(td))
      }),
      constructorData: t.constructorData->mapUpdate((id, cd) => {
        let newId = idMap->Gid.Map.get(id)->Option.getExn
        (newId, ConstructorData.duplicate(cd))
      }),
      edgeData: t.edgeData->mapUpdate((id, ed) => {
        let newId = idMap->Gid.Map.get(id)->Option.getExn
        let newSource = idMap->Gid.Map.get(EdgeData.source(ed))->Option.getExn
        let newTarget = idMap->Gid.Map.get(EdgeData.target(ed))->Option.getExn
        (
          newId,
          ed->EdgeData.duplicate->EdgeData.setSource(newSource)->EdgeData.setTarget(newTarget),
        )
      }),
      graph: t.graph->GraphState.duplicate(idMap),
    }
  }

  let updateMetadata = (t, f) => {...t, metadata: f(t.metadata)}
  let addToken = (t, id, ~x, ~y) => {
    let tokenData = TokenData.create("tok")
    let node = GraphState.GraphNode.create(id, ~x, ~y, GraphState.GraphNode.Token(tokenData))
    {
      ...t,
      tokenData: t.tokenData->Gid.Map.set(id, tokenData),
      graph: t.graph->GraphState.addNode(node),
    }
  }
  let addConstructor = (t, id, ~x, ~y) => {
    let constructorData = ConstructorData.create()
    let node = GraphState.GraphNode.create(
      id,
      ~x,
      ~y,
      GraphState.GraphNode.Constructor(constructorData),
    )
    {
      ...t,
      constructorData: t.constructorData->Gid.Map.set(id, constructorData),
      graph: t.graph->GraphState.addNode(node),
    }
  }
  let updateToken = (t, id, f) => {
    t.tokenData
    ->Gid.Map.get(id)
    ->Option.map(f)
    ->Option.map(newData => {
      ...t,
      tokenData: t.tokenData->Gid.Map.set(id, newData),
      graph: t.graph->GraphState.updateNode(
        id,
        GraphState.GraphNode.setData(_, GraphState.GraphNode.Token(newData)),
      ),
    })
    ->Option.getWithDefault(t)
  }

  let updateConstructor = (t, id, f) => {
    t.constructorData
    ->Gid.Map.get(id)
    ->Option.map(f)
    ->Option.map(newData => {
      ...t,
      constructorData: t.constructorData->Gid.Map.set(id, newData),
      graph: t.graph->GraphState.updateNode(
        id,
        GraphState.GraphNode.setData(_, GraphState.GraphNode.Constructor(newData)),
      ),
    })
    ->Option.getWithDefault(t)
  }

  let updateEdge = (t, id, f) => {
    t.edgeData
    ->Gid.Map.get(id)
    ->Option.map(f)
    ->Option.map(newData => {
      ...t,
      edgeData: t.edgeData->Gid.Map.set(id, newData),
      graph: t.graph->GraphState.updateLink(id, GraphState.GraphLink.setData(_, newData)),
    })
    ->Option.getWithDefault(t)
  }

  let moveNode = (t, id, ~x, ~y) => {
    ...t,
    graph: t.graph->GraphState.updateNode(id, GraphState.GraphNode.move(_, ~x, ~y)),
  }
  let duplicateNode = (t, ~oldId, ~newId) => {
    t
  }
  let connect = (t, ~linkId, ~source, ~target) => {
    let edgeData = EdgeData.create(source, target, None)
    let link = GraphState.GraphLink.create(linkId, ~source, ~target, ~edgeData)
    {
      ...t,
      edgeData: t.edgeData->Gid.Map.set(linkId, edgeData),
      graph: t.graph->GraphState.addLink(link),
    }
  }
  let deleteNode = (t, id) => {
    ...t,
    graph: t.graph->GraphState.deleteNode(id),
    tokenData: t.tokenData->Gid.Map.remove(id),
    constructorData: t.constructorData->Gid.Map.remove(id),
  }
  let deleteLink = (t, link) => {
    ...t,
    edgeData: t.edgeData->Gid.Map.remove(link),
    graph: t.graph->GraphState.deleteLink(link),
  }
  let setSelection = (t, selection) => {...t, graph: t.graph->GraphState.setSelection(selection)}

  let getNode = (t, id) =>
    if t.tokenData->Gid.Map.has(id) {
      Some(#token(t.tokenData->Gid.Map.get(id)->Option.getExn))
    } else if t.constructorData->Gid.Map.has(id) {
      Some(#constructor(t.constructorData->Gid.Map.get(id)->Option.getExn))
    } else {
      None
    }

  let getLink = (t, id) => t.edgeData->Gid.Map.get(id)
}

type t = {
  focused: option<Gid.t>,
  order: FileTree.t<Gid.t>,
  constructions: Gid.Map.t<UndoRedo.t<Construction.t>>,
  spaces: String.Map.t<CSpace.conSpec>,
  typeSystems: String.Map.t<FiniteSet.t<Type.PrincipalType.t>>,
}

let empty = {
  focused: None,
  order: FileTree.empty(),
  constructions: Gid.Map.empty(),
  spaces: String.Map.empty,
  typeSystems: String.Map.empty,
}

let focused = t => t.focused
let constructions = t =>
  t.order->FileTree.map(id =>
    t.constructions->Gid.Map.get(id)->Option.map(c => (id, UndoRedo.state(c)))->Option.getExn
  )
let construction = (t, id) => t.constructions->Gid.Map.get(id)->Option.map(UndoRedo.state)
let spaces = t => t.spaces
let typeSystems = t => t.typeSystems

let getAvailableSpaces: unit => Rpc.Response.t<array<CSpace.conSpec>> = Rpc_service.require(
  "server.spaces",
  Rpc.Datatype.unit_,
  Array.t_rpc(CSpace.conSpec_rpc),
)
let loadSpaces = t =>
  getAvailableSpaces()
  ->Rpc.Response.map(spaces => spaces->Array.map(space => (CSpace.conSpecName(space), space)))
  ->Rpc.Response.map(spaces => {
    ...t,
    spaces: spaces->String.Map.fromArray,
  })

let getAvailableTypeSystems: unit => Rpc.Response.t<
  array<(string, FiniteSet.t<Type.PrincipalType.t>)>,
> = Rpc_service.require(
  "server.typeSystems",
  Rpc.Datatype.unit_,
  Array.t_rpc(Rpc.Datatype.tuple2_(String.t_rpc, FiniteSet.t_rpc(Type.PrincipalType.t_rpc))),
)
let loadTypeSystems = t =>
  getAvailableTypeSystems()->Rpc.Response.map(systems => {
    ...t,
    typeSystems: systems->String.Map.fromArray,
  })

let load = () => None
let store = t => ()

let newConstruction = (t, id, name, path) => {
  let c = Construction.create(name)->UndoRedo.create
  {
    ...t,
    focused: Some(id),
    order: t.order->FileTree.insertFile(~path, ~position=-1, id)->Option.getExn,
    constructions: t.constructions->Gid.Map.set(id, c),
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
    ...t,
    focused: focused,
    order: t.order->FileTree.removeFile(id' => id' === id),
    constructions: t.constructions->Gid.Map.remove(id),
  }
}

let focusConstruction = (t, id) => {...t, focused: id}

let duplicateConstruction = (t, ~oldId, ~newId) => {
  let (path, position) = t.order->FileTree.getFilePathAndPosition(id => id === oldId)->Option.getExn
  t
  ->construction(oldId)
  ->Option.map(Construction.duplicate)
  ->Option.map(construction => {
    ...t,
    focused: Some(newId),
    order: t.order->FileTree.insertFile(~path, ~position=position + 1, newId)->Option.getExn,
    constructions: t.constructions->Gid.Map.set(newId, UndoRedo.create(construction)),
  })
  ->Option.getWithDefault(t)
}

let reorderConstructions = (t, order) => {...t, order: order}

let importConstruction = (t, id, construction, path) => {
  let c = Construction.duplicate(construction)->UndoRedo.create
  {
    ...t,
    focused: Some(id),
    order: t.order->FileTree.insertFile(~path, ~position=-1, id)->Option.getExn,
    constructions: t.constructions->Gid.Map.set(id, c),
  }
}

let updateConstruction = (t, id, f) => {
  ...t,
  constructions: t.constructions->Gid.Map.update(
    id,
    Option.map(_, ur_construction => {
      let c = UndoRedo.state(ur_construction)
      let c' = f(c)
      ur_construction->UndoRedo.step(c')
    }),
  ),
}

let updateConstructionBypassingUndoRedo = (t, id, f) => {
  ...t,
  constructions: t.constructions->Gid.Map.update(
    id,
    Option.map(_, ur_construction => {
      let c = UndoRedo.state(ur_construction)
      let c' = f(c)
      ur_construction->UndoRedo.replace(c')
    }),
  ),
}

let newFolder = (t, id, name, path) => {
  ...t,
  order: t.order
  ->FileTree.newFolder(~path, ~position=-1, ~name, ~id)
  ->Option.getWithDefault(t.order),
}

let renameFolder = (t, id, newName) => {
  ...t,
  order: t.order->FileTree.renameFolder(id, newName),
}

let deleteFolder = (t, id) => {
  ...t,
  order: t.order->FileTree.removeFolder(id),
}

let undo = (t, id) => {
  ...t,
  constructions: t.constructions->Gid.Map.update(id, Option.map(_, UndoRedo.undo)),
}
let redo = (t, id) => {
  ...t,
  constructions: t.constructions->Gid.Map.update(id, Option.map(_, UndoRedo.redo)),
}

let canUndo = (t, id) =>
  t.constructions->Gid.Map.get(id)->Option.map(UndoRedo.canUndo)->Option.getWithDefault(false)
let canRedo = (t, id) =>
  t.constructions->Gid.Map.get(id)->Option.map(UndoRedo.canRedo)->Option.getWithDefault(false)
