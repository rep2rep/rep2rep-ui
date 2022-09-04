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
    let hash = Hash.record2(("name", String.hash), ("notes", String.hash))

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

  let hash = Hash.record6(
    ("metadata", Metadata.hash),
    ("space", Option.hash(_, String.hash)),
    (
      "tokenData",
      toks =>
        toks
        ->Gid.Map.toArray
        ->Array.hash(((id, data)) => [Gid.hash(id), TokenData.hash(data)]->Hash.combine),
    ),
    (
      "constructorData",
      cons =>
        cons
        ->Gid.Map.toArray
        ->Array.hash(((id, data)) => [Gid.hash(id), ConstructorData.hash(data)]->Hash.combine),
    ),
    (
      "edgeData",
      edges =>
        edges
        ->Gid.Map.toArray
        ->Array.hash(((id, data)) => [Gid.hash(id), EdgeData.hash(data)]->Hash.combine),
    ),
    ("graph", GraphState.hash),
  )

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

  let toOruga = t => {
    let tokens = t.tokenData->Gid.Map.toArray
    let constructors = t.constructorData->Gid.Map.toArray
    let links = t.edgeData->Gid.Map.values
    let constructs =
      tokens->Array.filter(((id, _)) => links->Array.some(l => l->EdgeData.source === id)->not)
    let tokens = tokens->Gid.Map.fromArray
    let constructors = constructors->Gid.Map.fromArray

    let rec makeConstructions = (construct, usedTokens) => {
      switch usedTokens->Gid.Map.get(construct) {
      // If we've already seen this token, reference it.
      | Some(tok) => Or_error.create([(Constructions.Reference(tok), usedTokens)])
      | None => {
          let tok =
            tokens
            ->Gid.Map.get(construct)
            ->Or_error.fromOption_s("token data missing")
            ->Or_error.flatMap(td => CSpace.tokenFromTokenData(construct, td))
          let inputCons = links->Array.keepMap(l =>
            if EdgeData.target(l) === construct {
              Some((EdgeData.payload(l), EdgeData.source(l)))
            } else {
              None
            }
          )
          let usedTokens =
            tok
            ->Or_error.map(tok => usedTokens->Gid.Map.set(construct, tok))
            ->Or_error.getWithDefault(usedTokens)
          // If there's no "inputs" to this token, we call it a source
          if inputCons == [] {
            tok->Or_error.map(tok => [(Constructions.Source(tok), usedTokens)])
          } else {
            // Otherwise, for each possible constructor input...
            inputCons
            ->Array.map(((payload, cid)) => {
              let cons =
                constructors
                ->Gid.Map.get(cid)
                ->Option.flatMap(c => c.constructor)
                ->Or_error.fromOption_s("constructor data missing")
              (tok, cons)
              ->Or_error.both
              ->Or_error.flatMap(tc => {
                if Option.isSome(payload) {
                  Or_error.error_s("Arrows from constructors should be unlabelled")
                } else {
                  Or_error.create(tc)
                }
              })
              ->Or_error.flatMap(((tok, cons)) => {
                let tc = {
                  Constructions.token: tok,
                  constructor: cons,
                }
                // ... we find all input tokens...
                links
                ->Array.keepMap(l =>
                  if EdgeData.target(l) === cid {
                    EdgeData.payload(l)
                    ->Or_error.fromOption_s("Arrow has no number")
                    ->Or_error.flatMap(idx =>
                      if idx <= 0 {
                        Or_error.error_ss([
                          "Arrows into constructors must be labelled with positive numbers, not ",
                          Int.toString(idx),
                        ])
                      } else {
                        Or_error.create(idx)
                      }
                    )
                    ->Or_error.map(idx => (idx, EdgeData.source(l)))
                    ->Some
                  } else {
                    None
                  }
                )
                ->Or_error.allArray
                ->Or_error.flatMap(arrows => {
                  // Check that the incoming arrows are all unique
                  if (
                    arrows->Array.map(fst)->Belt.Set.Int.fromArray->Belt.Set.Int.size ===
                      Array.length(arrows)
                  ) {
                    Or_error.create(arrows)
                  } else {
                    Or_error.error_s("Arrows have duplicated values")
                  }
                })
                ->Or_error.flatMap(arrows => {
                  let count = Array.length(arrows)
                  if arrows->Array.map(fst)->Array.some(idx => idx > count) {
                    Or_error.error_s("Arrows into a constructor must be numbered 1 to n")
                  } else {
                    Or_error.create(arrows)
                  }
                })
                ->Or_error.flatMap(inputToks => {
                  inputToks->Js.Array2.sortInPlaceWith(((i, _), (j, _)) => i - j)->ignore
                  let inputToks = inputToks->Array.map(snd)
                  // ... then recursively find all ways to make those
                  let rec buildConstructionsForTokens = (i, usedTokens, result) => {
                    if i === Array.length(inputToks) {
                      [(Or_error.create(result), usedTokens)]
                    } else {
                      let tokId = inputToks[i]->Option.getExn
                      switch makeConstructions(tokId, usedTokens)->Or_error.match {
                      | Or_error.Ok(options) =>
                        options->Array.flatMap(((cons, usedTokens)) =>
                          buildConstructionsForTokens(i + 1, usedTokens, result->Array.push(cons))
                        )
                      | Or_error.Err(e) => [(Or_error.error(e), usedTokens)]
                      }
                    }
                  }
                  // We end up with all possible ways to build this constructor!
                  buildConstructionsForTokens(0, usedTokens, [])
                  ->Array.map(((inputConstructions, usedTokens)) =>
                    inputConstructions->Or_error.map(ic => (
                      Constructions.TCPair(tc, ic),
                      usedTokens,
                    ))
                  )
                  ->Or_error.allArray
                })
              })
            })
            // We then flatten all combinations of constructors and input tokens
            ->Or_error.allArray
            ->Or_error.map(Array.concatMany)
          }
        }
      }
    }

    if constructs == [] {
      Or_error.error_s("Structure graph has no constructs!")
    } else {
      constructs
      ->Array.map(((id, _)) => makeConstructions(id, Gid.Map.empty()))
      ->Or_error.allArray
      ->Or_error.map(Array.concatMany)
      ->Or_error.map(Array.map(_, fst))
    }
  }

  let fromOruga = (cons, ~space) => {
    Js.Console.log(cons)
    let ids = ref(String.Map.empty)
    let tokenData = ref(Gid.Map.empty())
    let constructorData = ref(Gid.Map.empty())
    let edgeData = ref(Gid.Map.empty())
    cons->Array.forEach(con => {
      let readOrugaType = ty => {
        Js.Console.log((ty, Type.split(ty)))
        switch Type.split(ty) {
        | [] => (None, None)
        | [a] => (Some(a), None)
        | arr => (Some(arr[1]->Option.getExn), Some(arr[0]->Option.getExn->Type.name))
        }
      }
      let handleToken = (tok, missingLink) => {
        let tname = CSpace.tokenName(tok)
        let ttyp = CSpace.tokenType(tok)
        let tid = switch ids.contents->String.Map.get(tname) {
        | Some(id) => id
        | None => {
            let id = Gid.create()
            ids := ids.contents->String.Map.set(tname, id)
            id
          }
        }
        switch tokenData.contents->Gid.Map.get(tid) {
        | Some(td) => () // Already added, we're fine!
        | None => {
            let (type_, subtype) = readOrugaType(ttyp)
            let td = TokenData.create(~type_?, ~subtype?, tname)
            tokenData := tokenData.contents->Gid.Map.set(tid, td)
          }
        }
        missingLink->Option.iter(((idx, connectTo)) => {
          let eid = Gid.create()
          let ed = EdgeData.create(tid, connectTo, Some(idx))
          edgeData := edgeData.contents->Gid.Map.set(eid, ed)
        })
        tid
      }
      let rec traverseCons = (c, missingLink) =>
        switch c {
        | Constructions.TCPair({token: tok, constructor: cons}, inputs) => {
            let tid = handleToken(tok, missingLink)
            let cd = {ConstructorData.constructor: Some(cons), notes: ""}
            let cid = Gid.create()
            constructorData := constructorData.contents->Gid.Map.set(cid, cd)
            let eid = Gid.create()
            let ed = EdgeData.create(cid, tid, None)
            edgeData := edgeData.contents->Gid.Map.set(eid, ed)
            inputs->Array.forEachWithIndex((idx, con) => traverseCons(con, Some(idx + 1, cid)))
          }
        | Constructions.Source(tok) => handleToken(tok, missingLink)->ignore
        | Constructions.Reference(tok) => handleToken(tok, missingLink)->ignore
        }
      traverseCons(con, None)
    })
    Or_error.create({
      metadata: {
        name: "Transformed construction",
        notes: "This construction is a result of applying structure transfer.",
      },
      space: Some(space),
      tokenData: tokenData.contents,
      constructorData: constructorData.contents,
      edgeData: edgeData.contents,
      graph: GraphState.layout(
        ~tokens=tokenData.contents,
        ~constructors=constructorData.contents,
        ~edges=edgeData.contents,
      ),
    })
  }

  let _transfer = Rpc_service.require(
    "server.transfer",
    Rpc.Datatype.tuple3_(Constructions.construction_rpc, String.t_rpc, String.t_rpc),
    Array.t_rpc(Constructions.construction_rpc),
  )

  let transfer = (t, ~targetSpace) => {
    let cons = t->toOruga
    let space = t.space->Or_error.fromOption_s("Structure graph is not part of a space")
    (cons, space)
    ->Or_error.both
    ->Or_error.flatMap(((cons, space)) =>
      switch cons {
      | [cons] =>
        (cons, space, targetSpace)
        ->_transfer
        ->Rpc.Response.map(fromOruga(_, ~space=targetSpace))
        ->Rpc.Response.map(cons =>
          cons->Or_error.map(cons => {
            ...cons,
            metadata: {
              ...cons.metadata,
              name: "TRANSFERRED " ++ t.metadata.name ++ " INTO " ++ targetSpace,
            },
          })
        )
        ->Or_error.create
      | _ => Or_error.error_s("Structure graph is not a construction.")
      }
    )
  }
}

type t = {
  focused: option<Gid.t>,
  order: FileTree.t<Gid.t>,
  constructions: Gid.Map.t<UndoRedo.t<Construction.t>>,
  spaces: String.Map.t<CSpace.conSpec>,
  typeSystems: String.Map.t<FiniteSet.t<Type.PrincipalType.t>>,
  renderers: String.Map.t<string>, // Endpoints for renderers by space
}

let hash = Hash.record6(
  ("focused", Option.hash(_, Gid.hash)),
  ("order", FileTree.hash(_, Gid.hash)),
  (
    "constructions",
    cons =>
      cons
      ->Gid.Map.toArray
      ->Array.map(((id, ur)) => (id, UndoRedo.state(ur)))
      ->Array.hash(((id, cons)) => [Gid.hash(id), Construction.hash(cons)]->Hash.combine),
  ),
  ("spaces", spaces => spaces->String.Map.keysToArray->Array.hash(String.hash)),
  ("typeSystems", tss => tss->String.Map.keysToArray->Array.hash(String.hash)),
  (
    "renderers",
    ps =>
      ps
      ->String.Map.toArray
      ->Array.hash(((k, v)) => Hash.combine([String.hash(k), String.hash(v)])),
  ),
)

let empty = {
  focused: None,
  order: FileTree.empty(),
  constructions: Gid.Map.empty(),
  spaces: String.Map.empty,
  typeSystems: String.Map.empty,
  renderers: String.Map.empty,
}

let focused = t => t.focused
let constructions = t =>
  t.order->FileTree.map(id =>
    t.constructions->Gid.Map.get(id)->Option.map(c => (id, UndoRedo.state(c)))->Option.getExn
  )
let construction = (t, id) => t.constructions->Gid.Map.get(id)->Option.map(UndoRedo.state)
let pathForConstruction = (t, id) =>
  t.order->FileTree.getFilePathAndPosition(id' => id == id')->Option.map(fst)
let spaces = t => t.spaces
let typeSystems = t => t.typeSystems
let getSpace = (t, name) => t.spaces->String.Map.get(name)
let renderable = (t, space) => t.renderers->String.Map.has(space)

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

let getAvailableRenderers: unit => Rpc.Response.t<array<(string, string)>> = Rpc_service.require(
  "server.renderers",
  Rpc.Datatype.unit_,
  Array.t_rpc(Rpc.Datatype.tuple2_(String.t_rpc, String.t_rpc)),
)
let loadRenderers = t =>
  getAvailableRenderers()->Rpc.Response.map(renderers => {
    ...t,
    renderers: renderers->String.Map.fromArray,
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
      renderers: String.Map.empty,
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

let renderConstruction = (t, id) =>
  t.constructions
  ->Gid.Map.get(id)
  ->Or_error.fromOption_s("Cannot find construction to render")
  ->Or_error.flatMap(c => {
    let c = UndoRedo.state(c)
    c
    ->Construction.space
    ->Option.flatMap(t.renderers->String.Map.get)
    ->Or_error.fromOption_s("Construction Space is not renderable")
    ->Or_error.flatMap(endpoint => {
      let f = Rpc_service.require(
        endpoint,
        Constructions.construction_rpc,
        Array.t_rpc(
          Rpc.Datatype.tuple2_(
            String.t_rpc,
            Rpc.Datatype.tuple3_(String.t_rpc, Float.t_rpc, Float.t_rpc),
          ),
        ),
      )
      c
      ->Construction.toOruga
      ->Or_error.flatMap(cs =>
        switch cs {
        | [o] =>
          o
          ->f
          ->Rpc.Response.map(renderedToks => {
            let c =
              c
              ->Construction.tokens
              ->Array.reduce(c, (c', id) =>
                c'->Construction.updateToken(id, TokenData.setPayload(_, None))
              )
            renderedToks->Array.reduce(c, (c', (tokId, payload)) =>
              c'->Construction.updateToken(
                tokId->Gid.fromString,
                TokenData.setPayload(_, Some(payload)),
              )
            )
          })
          ->Or_error.create
        | _ => Or_error.error_s("Not a construction!")
        }
      )
    })
  })

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
