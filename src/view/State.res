let db = SetOnce.create()

module Construction = {
  module Metadata = {
    type t = {
      name: string,
      notes: string,
    }

    module Stable = {
      module V1 = {
        type t = t = {
          name: string,
          notes: string,
        }

        let toJson = t =>
          Js.Dict.fromArray([
            ("version", 1->Int.toJson),
            ("name", t.name->String.toJson),
            ("notes", t.notes->String.toJson),
          ])->Js.Json.object_

        let fromJson = json =>
          json
          ->Js.Json.decodeObject
          ->Or_error.fromOption_s("Failed to decode Construction Metadata state object JSON")
          ->Or_error.flatMap(dict => {
            let getValue = (key, reader) =>
              dict
              ->Js.Dict.get(key)
              ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
              ->Or_error.flatMap(reader)
            let version = getValue("version", Int.fromJson)
            switch version->Or_error.match {
            | Or_error.Ok(1) => {
                let name = getValue("name", String.fromJson)
                let notes = getValue("notes", String.fromJson)
                (name, notes)
                ->Or_error.both
                ->Or_error.map(((name, notes)) => {
                  name: name,
                  notes: notes,
                })
              }
            | Or_error.Ok(v) =>
              Or_error.error_ss(["Unknown Construction Metadata version ", Int.toString(v)])
            | Or_error.Err(e) => Or_error.error(e)
            }
          })
      }
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
    space: option<string>,
    tokenData: Gid.Map.t<TokenData.t>,
    constructorData: Gid.Map.t<ConstructorData.t>,
    edgeData: Gid.Map.t<EdgeData.t>,
    graph: GraphState.t,
  }

  module Stable = {
    module V1 = {
      type t = t = {
        metadata: Metadata.Stable.V1.t,
        space: option<string>,
        tokenData: Gid.Map.t<TokenData.Stable.V1.t>,
        constructorData: Gid.Map.t<ConstructorData.Stable.V1.t>,
        edgeData: Gid.Map.t<EdgeData.Stable.V1.t>,
        graph: GraphState.Stable.V1.t,
      }

      let toJson = t =>
        Js.Dict.fromArray([
          ("version", 1->Int.toJson),
          ("metadata", t.metadata->Metadata.Stable.V1.toJson),
          ("space", t.space->Option.toJson(String.toJson)),
          ("tokenData", t.tokenData->Gid.Map.toJson(TokenData.Stable.V1.toJson)),
          ("constructorData", t.constructorData->Gid.Map.toJson(ConstructorData.Stable.V1.toJson)),
          ("edgeData", t.edgeData->Gid.Map.toJson(EdgeData.Stable.V1.toJson)),
          ("graph", t.graph->GraphState.Stable.V1.toJson),
        ])->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Construction state object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let metadata = getValue("metadata", Metadata.Stable.V1.fromJson)
              let space = getValue("space", Option.fromJson(_, String.fromJson))
              let tokenData = getValue(
                "tokenData",
                Gid.Map.fromJson(_, TokenData.Stable.V1.fromJson),
              )
              let constructorData = getValue(
                "constructorData",
                Gid.Map.fromJson(_, ConstructorData.Stable.V1.fromJson),
              )
              let edgeData = getValue("edgeData", Gid.Map.fromJson(_, EdgeData.Stable.V1.fromJson))
              let graph = getValue("graph", GraphState.Stable.V1.fromJson)
              (metadata, space, tokenData, constructorData, edgeData, graph)
              ->Or_error.both6
              ->Or_error.map(((metadata, space, tokenData, constructorData, edgeData, graph)) => {
                metadata: metadata,
                space: space,
                tokenData: tokenData,
                constructorData: constructorData,
                edgeData: edgeData,
                graph: graph,
              })
            }
          | Or_error.Ok(v) => Or_error.error_ss(["Unknown Construction version ", Int.toString(v)])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }

  module StorageMkr = (J: LocalStorage.Jsonable) => {
    let set = (key, value) => {
      let str = J.toJson(value)->Js.Json.stringify
      switch db->SetOnce.get {
      | None => Dialog.alert("Failed to save! Couldn't connect to database")
      | Some((db, store)) =>
        db
        ->IndexedDB.put(~store, ~key, str)
        ->Promise.catch(_ => {
          Dialog.alert("Failed to save! Couldn't write data")
          Promise.resolve(str)
        })
        ->ignore
      }
    }

    let get = key => {
      switch db->SetOnce.get {
      | None =>
        Or_error.error_s("Failed to read model - could not connect to database!")->Promise.resolve
      | Some((db, store)) =>
        db
        ->IndexedDB.get(~store, ~key)
        ->Promise.thenResolve(s => s->Js.Json.parseExn->J.fromJson)
        ->Promise.catch(_ => {
          let msg = "Failed to load model " ++ key ++ "."
          Js.Console.log(msg)
          Promise.resolve(Or_error.error_s(msg))
        })
      }
    }

    let delete = key => {
      switch db->SetOnce.get {
      | None => Dialog.alert("Failed to delete model - could not connect to database!")
      | Some((db, store)) =>
        db
        ->IndexedDB.delete(~store, ~key)
        ->Promise.catch(e => {
          Js.Console.log(e)
          Dialog.alert("Failed to delete model!")
          Promise.resolve()
        })
        ->ignore
      }
    }
  }
  module Storage = StorageMkr(Stable.V1)

  let graph = t => t.graph
  let metadata = t => t.metadata
  let space = t => t.space
  let tokens = t => t.tokenData->Gid.Map.keys
  let constructors = t => t.constructorData->Gid.Map.keys
  let links = t => t.edgeData->Gid.Map.keys

  let create = name => {
    metadata: Metadata.create(name),
    space: None,
    tokenData: Gid.Map.empty(),
    constructorData: Gid.Map.empty(),
    edgeData: Gid.Map.empty(),
    graph: GraphState.empty,
  }

  let prefix = "RepNotation:Model:"
  let store = (t, id) => Storage.set(prefix ++ Gid.toString(id), t)
  let load = id => Storage.get(prefix ++ Gid.toString(id))
  let delete = id => Storage.delete(prefix ++ Gid.toString(id))

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
    let idMap =
      Gid.Map.merge(t.tokenData, t.constructorData, (_, _, _) => Some(0))->Gid.Map.merge(
        t.edgeData,
        (_, _, _) => Some(Gid.create()),
      )
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
let getSpace = (t, name) => t.spaces->String.Map.get(name)

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

let setDB = (newDB, store) => db->SetOnce.set((newDB, store))

let load = () => {
  let focused = LocalStorage.Raw.getItem("RST:Focused")->Option.flatMap(s => {
    let json = try Or_error.create(Js.Json.parseExn(s)) catch {
    | _ => Or_error.error_s("Badly stored Focused")
    }
    json->Or_error.flatMap(json => json->Option.fromJson(Gid.fromJson))->Or_error.toOption
  })
  let order = LocalStorage.Raw.getItem("RST:Order")->Option.flatMap(s => {
    let json = try Or_error.create(Js.Json.parseExn(s)) catch {
    | _ => Or_error.error_s("Badly stored Order")
    }
    json
    ->Or_error.flatMap(json => json->FileTree.Stable.V2.fromJson(Gid.fromJson))
    ->Or_error.toOption
  })
  let constructions =
    order
    ->Option.map(order => {
      order
      ->FileTree.flatten
      ->Array.map(id => Construction.load(id)->Promise.thenResolve(c => (id, c)))
      ->Promise.all
      ->Promise.thenResolve(arr =>
        arr
        ->Array.keepMap(((id, model)) =>
          switch model->Or_error.match {
          | Or_error.Ok(m) => (id, UndoRedo.create(m))->Some
          | Or_error.Err(e) => {
              Dialog.alert(
                "Error loading construction: " ++ Error.messages(e)->Js.Array2.joinWith(";"),
              )
              None
            }
          }
        )
        ->Gid.Map.fromArray
        ->Some
      )
    })
    ->Option.getWithDefault(Promise.resolve(None))

  constructions->Promise.thenResolve(constructions => {
    Option.both3((focused, order, constructions))->Option.map(((focused, order, constructions)) => {
      focused: focused,
      order: order,
      constructions: constructions,
      spaces: String.Map.empty,
      typeSystems: String.Map.empty,
    })
  })
}

let store = t => {
  LocalStorage.Raw.setItem("RST:Focused", t.focused->Option.toJson(Gid.toJson)->Js.Json.stringify)
  LocalStorage.Raw.setItem(
    "RST:Order",
    t.order->FileTree.Stable.V2.toJson(Gid.toJson)->Js.Json.stringify,
  )
  t.constructions->Gid.Map.forEach((id, construction) =>
    Construction.store(construction->UndoRedo.state, id)
  )
}

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
  Construction.delete(id)
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
