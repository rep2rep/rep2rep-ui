module BoolStore = LocalStorage.MakeJsonable(Bool)
module K = GlobalKeybindings.KeyBinding
module FP = {
  include FilePanel
  let make = React.memo(make)
}

module App = {
  let db_store = "RST"
  let db_ready = IndexedDB.open_(~name="rst", ~version=1, ~onUpgradeNeeded=db =>
    db->IndexedDB.createObjectStore(db_store)
  )->Promise.thenResolve(db => {
    db->IndexedDB.onError(e => {
      Js.Console.log(e)
      Dialog.alert("Database Error!")
    })
    State.setDB(db, db_store)
  })
  let init =
    db_ready
    ->Promise.then(_ => State.load())
    ->Promise.thenResolve(s => s->Option.getWithDefault(State.empty))
  let reducer = (state, action) => {
    let newState = Event.dispatch(state, action)
    State.store(newState)
    newState
  }

  let config = ReactD3Graph.Config.create(
    ~global=ReactD3Graph.Config.Global.create(
      ~width="100%",
      ~height="calc(100vh - 40px)",
      ~defs=[
        <marker id="arrowhead" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto">
          <path d="M1 1 L7 4 L1 7 Z" fill="black" strokeWidth="1" stroke="#000000" />
        </marker>,
      ],
      (),
    ),
    ~d3=ReactD3Graph.Config.D3.create(~disableLinkForce=true, ()),
    ~node=ReactD3Graph.Node.Config.create(~renderLabel=false, ()),
    ~link=ReactD3Graph.Link.Config.create(
      ~color=ReactD3Graph.Color.ofHexString("#000000"),
      ~renderLabel=true,
      ~fontSize=10.0,
      ~fontScaling=false,
      ~curveType=ReactD3Graph.Link.CurveType.catmullRom,
      ~strokeWidth=1.,
      ~markerEnd="arrowhead",
      (),
    ),
    (),
  )

  @react.component
  let make = (~init) => {
    let (state, dispatch) = React.useReducer(reducer, init)

    React.useEffect0(() => {
      state
      ->State.loadSpaces
      ->Rpc.Response.flatMap(State.loadTypeSystems)
      ->Rpc.Response.flatMap(State.loadRenderers)
      ->Rpc.Response.upon(state => {
        dispatch(Event.Update(state))
      })
      None
    })

    let focused = state->State.focused

    let selection =
      focused
      ->Option.flatMap(state->State.construction(_))
      ->Option.map(State.Construction.graph)
      ->Option.map(GraphState.selection)
      ->Option.getWithDefault(GraphState.Selection.empty)

    let dispatchC = e =>
      focused->Option.iter(focused => dispatch(Event.ConstructionEvent(focused, e)))

    let newConstruction = path =>
      dispatch(Event.NewConstruction(Gid.create(), "Structure graph", path))
    let deleteConstruction = id => dispatch(Event.DeleteConstruction(id))
    let focusConstruction = id => dispatch(Event.FocusConstruction(id))
    let duplicateConstruction = id => {
      let newId = Gid.create()
      let newName =
        state
        ->State.construction(id)
        ->Option.map(c =>
          c->State.Construction.metadata->State.Construction.Metadata.name ++ " (Copy)"
        )
      dispatch(Event.DuplicateConstruction(id, newId))
      newName->Option.iter(newName =>
        dispatch(
          Event.ConstructionEvent(
            newId,
            Event.Construction.UpdateMetadata(Event.Construction.Metadata.Name(newName)),
          ),
        )
      )
    }
    let renameConstruction = (id, newName) =>
      dispatch(
        Event.ConstructionEvent(
          id,
          Event.Construction.UpdateMetadata(Event.Construction.Metadata.Name(newName)),
        ),
      )
    let reorderConstructions = newOrder => dispatch(Event.ReorderConstructions(newOrder))
    let importConstructions = (fs, path) =>
      fs->Array.forEach(f => {
        File.text(f)
        |> Js.Promise.then_(text => {
          let construction = try text
          ->Js.Json.parseExn
          ->State.Construction.Stable.V1.fromJson catch {
          | _ => Or_error.error_s("fail")
          }
          switch construction->Or_error.match {
          | Or_error.Ok(construction) => {
              let construction =
                construction->State.Construction.updateMetadata(m =>
                  m->State.Construction.Metadata.setNotes(
                    m->State.Construction.Metadata.notes ++
                    "\n*** Imported " ++
                    Js.Date.make()->Js.Date.toString ++ " ***",
                  )
                )
              dispatch(Event.ImportConstruction(Gid.create(), construction, path))
            }
          | Or_error.Err(e) => {
              Js.Console.log(e)
              Dialog.alert("Failed to import '" ++ File.name(f) ++ "'.")
            }
          }
          Js.Promise.resolve()
        })
        |> ignore
      })

    let exportConstruction = id => {
      state
      ->State.construction(id)
      ->Option.iter(construction => {
        let construction =
          construction->State.Construction.updateMetadata(m =>
            m->State.Construction.Metadata.setNotes(
              m->State.Construction.Metadata.notes ++
              "\n=== Exported " ++
              Js.Date.make()->Js.Date.toString ++ " ===",
            )
          )
        let name = construction->State.Construction.metadata->State.Construction.Metadata.name
        let json = State.Construction.Stable.V1.toJson(construction)
        let content =
          "data:text/json;charset=utf-8," ++ json->Js.Json.stringify->Js.Global.encodeURIComponent
        Downloader.download(name ++ ".rst", content)
      })
    }

    let renderConstruction = id =>
      switch state->State.renderConstruction(id)->Or_error.match {
      | Or_error.Err(e) => Js.Console.log(e)
      | Or_error.Ok(construction) =>
        construction->Rpc.Response.upon(c => dispatchC(Event.Construction.Replace(c)))
      }

    let createFolder = path => dispatch(Event.NewFolder(Gid.create(), "Folder", path))
    let renameFolder = (id, newName) => dispatch(Event.RenameFolder(id, newName))
    let deleteFolder = id => dispatch(Event.DeleteFolder(id))

    let canUndo = focused->Option.map(State.canUndo(state))->Option.getWithDefault(false)
    let canRedo = focused->Option.map(State.canRedo(state))->Option.getWithDefault(false)
    let undo = _ => focused->Option.iter(focused => dispatch(Event.Undo(focused)))
    let redo = _ => focused->Option.iter(focused => dispatch(Event.Redo(focused)))

    let toolbarActive = focused->Option.isSome

    let addTokenNodeAt = (_, ~x, ~y, ~reversed) => {
      let tok = Gid.create()
      switch GraphState.Selection.nodes(selection) {
      | [] => dispatchC(Event.Construction.AddToken(tok, x, y))
      | inputs =>
        focused
        ->Option.flatMap(State.construction(state, _))
        ->Option.iter(construction => {
          let kind = vert =>
            construction
            ->State.Construction.getNode(vert)
            ->Option.map(v =>
              switch v {
              | #token(_) => #token
              | #constructor(_) => #constructor
              }
            )
          if inputs->Array.every(i => kind(i) === Some(#constructor)) {
            let e0 = Event.Construction.AddToken(tok, x, y)
            let es = if reversed {
              inputs->Array.flatMapWithIndex((i, cons) => {
                let arrowId = Gid.create()
                let mkArr = Event.Construction.ConnectNodes(arrowId, tok, cons)
                let labelArr = Event.Construction.UpdateEdge(arrowId, Event.Edge.Value(Some(i + 1)))
                [mkArr, labelArr]
              })
            } else {
              inputs->Array.map(cons => {
                Event.Construction.ConnectNodes(Gid.create(), cons, tok)
              })
            }
            dispatchC(Event.Construction.Multiple(Array.concat([e0], es)))
          } else {
            dispatchC(Event.Construction.AddToken(tok, x, y))
          }
        })
      }
      dispatchC(Event.Construction.ChangeSelection(GraphState.Selection.ofNodes([tok])))
    }

    let addConstructorNodeAt = (_, ~x, ~y, ~reversed) => {
      let cons = Gid.create()
      switch GraphState.Selection.nodes(selection) {
      | [] => dispatchC(Event.Construction.AddConstructor(cons, x, y))
      | inputs =>
        focused
        ->Option.flatMap(State.construction(state, _))
        ->Option.iter(construction => {
          let kind = vert =>
            construction
            ->State.Construction.getNode(vert)
            ->Option.map(v =>
              switch v {
              | #token(_) => #token
              | #constructor(_) => #constructor
              }
            )
          if inputs->Array.every(i => kind(i) === Some(#token)) {
            let e0 = Event.Construction.AddConstructor(cons, x, y)
            let es = if reversed {
              inputs->Array.map(tok => {
                Event.Construction.ConnectNodes(Gid.create(), cons, tok)
              })
            } else {
              inputs->Array.flatMapWithIndex((i, tok) => {
                let arrowId = Gid.create()
                let addArr = Event.Construction.ConnectNodes(arrowId, tok, cons)
                let numberArr = Event.Construction.UpdateEdge(
                  arrowId,
                  Event.Edge.Value(Some(i + 1)),
                )
                [addArr, numberArr]
              })
            }
            dispatchC(Event.Construction.Multiple(Array.concat([e0], es)))
          } else {
            dispatchC(Event.Construction.AddConstructor(cons, x, y))
          }
        })
      }
      dispatchC(Event.Construction.ChangeSelection(GraphState.Selection.ofNodes([cons])))
    }

    let connectNodes = (_, ~reversed) =>
      switch GraphState.Selection.nodes(selection) {
      | [] | [_] => ()
      | nodes =>
        focused
        ->Option.flatMap(State.construction(state, _))
        ->Option.iter(construction => {
          let kind = vert =>
            construction
            ->State.Construction.getNode(vert)
            ->Option.map(v =>
              switch v {
              | #token(_) => #token
              | #constructor(_) => #constructor
              }
            )
          let (sources, targets) = {
            // We have three happy cases, the rest bad:
            // [a, b, ..., b]
            // [a, ..., a, b]
            // [a, b]
            // (Note that order doesn't matter, except for determining the direction)
            switch nodes {
            | [] | [_] => ([], []) // Previously eliminated, but safer this way!
            | [source, target] => ([source], [target])
            | _ => {
                let (toks, cons) = nodes->Array.partition(v => kind(v) === Some(#token))
                let k1 = nodes[0]->Option.flatMap(kind)
                if k1 === Some(#token) {
                  (toks, cons)
                } else {
                  (cons, toks)
                }
              }
            }
          }
          let (sources, targets) = if reversed {
            (targets, sources)
          } else {
            (sources, targets)
          }
          if Array.length(sources) !== 0 && Array.length(targets) !== 0 {
            let s = sources[0]->Option.flatMap(kind)
            if s === Some(#token) {
              let es = sources->Array.flatMapWithIndex((i, source) =>
                targets->Array.flatMap(target => {
                  let arrId = Gid.create()
                  let e1 = Event.Construction.ConnectNodes(arrId, source, target)
                  let e2 = Event.Construction.UpdateEdge(arrId, Event.Edge.Value(Some(i + 1)))
                  [e1, e2]
                })
              )
              dispatchC(Event.Construction.Multiple(es))
            } else {
              // If the source is a constructor, it can only have one output!
              // However, showing no edges would be confusing, so we add them all anyway...
              let es = sources->Array.flatMap(source =>
                targets->Array.map(target => {
                  Event.Construction.ConnectNodes(Gid.create(), source, target)
                })
              )
              dispatchC(Event.Construction.Multiple(es))
            }
          }
        })
      }

    let magicNumber = (_, ~x, ~y, n) =>
      focused
      ->Option.flatMap(State.construction(state, _))
      ->Option.iter(construction => {
        let kind = vert =>
          construction
          ->State.Construction.getNode(vert)
          ->Option.map(v =>
            switch v {
            | #token(_) => #token
            | #constructor(_) => #constructor
            }
          )
        switch GraphState.Selection.nodes(selection) {
        | [] =>
          if n > 0 {
            // Behaviour: Add a new constructor, an output token, and "n" input tokens
            let cons = Gid.create()
            let out = Gid.create()
            let x_space =
              (120. *. Js.Math.pow_float(~base=0.8, ~exp=Int.toFloat(n)))->Js.Math.max_float(35.)
            let y_space = 70.
            let high_y = y -. y_space
            let low_y = y +. y_space
            let width = Int.toFloat(n - 1) *. x_space
            let nodes = [cons, out]
            let e_in = Array.range(1, n)->Array.flatMap(i => {
              let tok = Gid.create()
              nodes->Js.Array2.push(tok)->ignore
              let arr = Gid.create()
              let new_x = Int.toFloat(i - 1) *. x_space +. x -. width /. 2.
              [
                Event.Construction.AddToken(tok, new_x, low_y),
                Event.Construction.ConnectNodes(arr, tok, cons),
                Event.Construction.UpdateEdge(arr, Event.Edge.Value(Some(i))),
              ]
            })
            let es =
              [
                Event.Construction.AddConstructor(cons, x +. 5., y),
                Event.Construction.AddToken(out, x, high_y),
                Event.Construction.ConnectNodes(Gid.create(), cons, out),
              ]
              ->Array.concat(e_in)
              ->Array.push(Event.Construction.ChangeSelection(GraphState.Selection.ofNodes(nodes)))
            dispatchC(Event.Construction.Multiple(es))
          }
        | [node] =>
          switch kind(node) {
          | None => ()
          | Some(#token) =>
            if n === 0 {
              // Behaviour: The selected token is the output of a new constructor
              let cons = Gid.create()
              let arr = Gid.create()
              let es = [
                Event.Construction.AddConstructor(cons, x, y),
                Event.Construction.ConnectNodes(arr, cons, node),
                Event.Construction.ChangeSelection(GraphState.Selection.ofNodes([cons])),
              ]
              dispatchC(Event.Construction.Multiple(es))
            } else {
              // Behaviour: The selected token is the "nth" input to a new constructor
              let cons = Gid.create()
              let arr = Gid.create()
              let es = [
                Event.Construction.AddConstructor(cons, x, y),
                Event.Construction.ConnectNodes(arr, node, cons),
                Event.Construction.UpdateEdge(arr, Event.Edge.Value(Some(n))),
                Event.Construction.ChangeSelection(GraphState.Selection.ofNodes([cons])),
              ]
              dispatchC(Event.Construction.Multiple(es))
            }
          | Some(#constructor) =>
            if n === 0 {
              // Behaviour: The selected construction feeds into a new token
              let tok = Gid.create()
              let arr = Gid.create()
              let es = [
                Event.Construction.AddToken(tok, x, y),
                Event.Construction.ConnectNodes(arr, node, tok),
                Event.Construction.ChangeSelection(GraphState.Selection.ofNodes([tok])),
              ]
              dispatchC(Event.Construction.Multiple(es))
            } else {
              // Behaviour: The selected construction has the new token as the "nth" input
              let tok = Gid.create()
              let arr = Gid.create()
              let es = [
                Event.Construction.AddToken(tok, x, y),
                Event.Construction.ConnectNodes(arr, tok, node),
                Event.Construction.UpdateEdge(arr, Event.Edge.Value(Some(n))),
                Event.Construction.ChangeSelection(GraphState.Selection.ofNodes([tok])),
              ]
              dispatchC(Event.Construction.Multiple(es))
            }
          }
        | [source, target] =>
          if n === 0 {
            // Behaviour: connect the constructor to the token
            let s = kind(source)
            let t = kind(target)
            let e = if s === Some(#token) && t == Some(#constructor) {
              Event.Construction.ConnectNodes(Gid.create(), target, source)->Some
            } else if s === Some(#constructor) && t == Some(#token) {
              Event.Construction.ConnectNodes(Gid.create(), source, target)->Some
            } else {
              None
            }
            e->Option.iter(dispatchC)
          } else {
            // Behaviour: Connect the token to the constructor as "nth" arrow
            let s = kind(source)
            let t = kind(target)
            let arr = Gid.create()
            let es = if s === Some(#token) && t == Some(#constructor) {
              [
                Event.Construction.ConnectNodes(arr, source, target),
                Event.Construction.UpdateEdge(arr, Event.Edge.Value(Some(n))),
              ]->Some
            } else if s === Some(#constructor) && t == Some(#token) {
              [
                Event.Construction.ConnectNodes(arr, target, source),
                Event.Construction.UpdateEdge(arr, Event.Edge.Value(Some(n))),
              ]->Some
            } else {
              None
            }
            es->Option.iter(es => dispatchC(Event.Construction.Multiple(es)))
          }
        | _ => ()
        }
      })

    let deleteSelection = _ => {
      let ds =
        selection
        ->GraphState.Selection.nodes
        ->Array.flatMap(nodeId => {
          let ds =
            focused
            ->Option.flatMap(State.construction(state, _))
            ->Option.map(State.Construction.graph)
            ->Option.map(GraphState.incidentLinks(_, ~nodeId))
            ->Option.map(o => Array.concat(o["in"], o["out"]))
            ->Option.map(Array.map(_, linkId => Event.Construction.DeleteLink(linkId)))
            ->Option.getWithDefault([])
          Array.concatMany([ds, [Event.Construction.DeleteNode(nodeId)]])
        })
      let es =
        selection
        ->GraphState.Selection.links
        ->Array.map(linkId => Event.Construction.DeleteLink(linkId))
      let unselect = Event.Construction.ChangeSelection(GraphState.Selection.empty)
      dispatchC(Event.Construction.Multiple(Array.concatMany([ds, es, [unselect]])))
    }

    let movedNode = (id, ~x, ~y) =>
      dispatchC(
        Event.Construction.MoveNode(id->ReactD3Graph.Node.Id.toString->Gid.fromString, x, y),
      )

    let selectionChange = (~oldSelection as _, ~newSelection) =>
      dispatchC(
        Event.Construction.ChangeSelection(GraphState.Selection.fromReactD3Selection(newSelection)),
      )

    let inspectorChange = e => dispatchC(e)

    GlobalKeybindings.set([
      K.create(K.cmdOrCtrl() ++ "+z", undo),
      K.create(K.cmdOrCtrl() ++ "+Shift+z", redo),
      K.create(K.cmdOrCtrl() ++ "+y", redo),
    ])

    let keybindings = Js.Dict.fromArray(
      Array.concat(
        [
          ("t", (e, ~x, ~y) => addTokenNodeAt(e, ~x, ~y, ~reversed=false)),
          ("Shift+T", (e, ~x, ~y) => addTokenNodeAt(e, ~x, ~y, ~reversed=true)),
          ("c", (e, ~x, ~y) => addConstructorNodeAt(e, ~x, ~y, ~reversed=false)),
          ("Shift+C", (e, ~x, ~y) => addConstructorNodeAt(e, ~x, ~y, ~reversed=true)),
          ("e", (e, ~x as _, ~y as _) => connectNodes(e, ~reversed=false)),
          ("Shift+E", (e, ~x as _, ~y as _) => connectNodes(e, ~reversed=true)),
          ("a", (e, ~x as _, ~y as _) => connectNodes(e, ~reversed=false)),
          ("Shift+A", (e, ~x as _, ~y as _) => connectNodes(e, ~reversed=true)),
          ("x", (e, ~x as _, ~y as _) => deleteSelection(e)),
          ("Backspace", (e, ~x as _, ~y as _) => deleteSelection(e)),
          ("Delete", (e, ~x as _, ~y as _) => deleteSelection(e)),
        ],
        Array.range(0, 9)->Array.map(i => (
          Int.toString(i),
          (e, ~x, ~y) => magicNumber(e, ~x, ~y, i),
        )),
      ),
    )

    let (showGrid, setShowGrid) = React.useState(_ => {
      BoolStore.get("REP-SHOW-GRID")->Or_error.getWithDefault(false)
    })

    let toggleGrid = _ => {
      if showGrid {
        BoolStore.set("REP-SHOW-GRID", false)
        setShowGrid(_ => false)
      } else {
        BoolStore.set("REP-SHOW-GRID", true)
        setShowGrid(_ => true)
      }
    }

    <main
      style={ReactDOM.Style.make(
        ~display="flex",
        ~flexDirection="row",
        ~fontFamily="sans-serif",
        ~height="100%",
        ~overflow="hidden",
        (),
      )}>
      <FP
        id="file-panel"
        data={State.constructions(state)}
        dataName={construction =>
          construction->State.Construction.metadata->State.Construction.Metadata.name}
        title="RST Editor"
        version="##VERSION##"
        importExtensions=[".rst"]
        active={State.focused(state)}
        onCreate={newConstruction}
        onDelete={deleteConstruction}
        onSelect={focusConstruction}
        onDuplicate={duplicateConstruction}
        onChangedName={renameConstruction}
        onReorder={reorderConstructions}
        onImport={importConstructions}
        onExport={exportConstruction}
        onCreateFolder={createFolder}
        onDeleteFolder={deleteFolder}
        onChangedFolderName={renameFolder}
      />
      <div
        className="editor-panel"
        style={ReactDOM.Style.make(
          ~order="2",
          ~flexGrow="1",
          ~display="flex",
          ~flexDirection="column",
          ~height="100%",
          ~overflow="hidden",
          (),
        )}>
        <div
          className="graph-header"
          style={ReactDOM.Style.make(
            ~order="1",
            ~display="flex",
            ~alignItems="center",
            ~minHeight="30px",
            ~borderBottom="1px solid black",
            ~padding="0 0.5rem",
            ~overflowX="auto",
            (),
          )}>
          <Button onClick={undo} value="Undo" enabled={canUndo} tooltip="Cmd+Z" />
          <Button onClick={redo} value="Redo" enabled={canRedo} tooltip="Cmd+Shift+Z" />
          <Button.Separator />
          <label htmlFor="space-selector" style={ReactDOM.Style.make(~marginRight="0.25rem", ())}>
            {React.string("Space ")}
          </label>
          <Inspector.Selector
            name="space-selector"
            options={String.Map.keysToArray(State.spaces(state))}
            current={focused
            ->Option.flatMap(state->State.construction(_))
            ->Option.flatMap(State.Construction.space)}
            toString={space => space}
            fromString={space =>
              if State.spaces(state)->String.Map.has(space) {
                Some(space)
              } else {
                None
              }}
            onChange={s => Event.Construction.SetSpace(s)->dispatchC}
            enabled={focused
            ->Option.flatMap(state->State.construction(_))
            ->Option.map(State.Construction.isEmpty)
            ->Option.getWithDefault(true) && toolbarActive}
          />
          <Button.Separator />
          <Button
            onClick={addTokenNodeAt(_, ~x=0., ~y=0., ~reversed=false)}
            value="Token"
            enabled={toolbarActive}
            tooltip="T"
          />
          <Button
            onClick={addConstructorNodeAt(_, ~x=0., ~y=0., ~reversed=false)}
            value="Constructor"
            enabled={toolbarActive}
            tooltip="C"
          />
          // <Button
          //   onClick={duplicateNodes} value="Duplicate" enabled={toolbarActive} tooltip="Ctrl+D"
          // />
          <Button.Separator />
          <Button
            onClick={connectNodes(_, ~reversed=false)}
            value="Connect"
            enabled={toolbarActive}
            tooltip="E"
          />
          <Button.Separator />
          <Button onClick={deleteSelection} value="Delete" enabled={toolbarActive} tooltip="X" />
          <Button.Separator />
          <label htmlFor="gridToggle"> {React.string("Grid")} </label>
          <input
            type_="checkbox"
            label="gridToggle"
            onChange={toggleGrid}
            checked={showGrid}
            style={ReactDOM.Style.make(~marginLeft="0.5em", ())}
          />
          <Button.Separator />
          <Button
            onClick={_ => focused->Option.iter(renderConstruction)}
            value="Render"
            enabled={toolbarActive &&
            focused
            ->Option.flatMap(state->State.construction(_))
            ->Option.flatMap(State.Construction.space)
            ->Option.map(state->State.renderable(_))
            ->Option.getWithDefault(false)}
          />
          // <Button.Separator />
          // <a href="manual.html" target="_blank"> {React.string("Manual")} </a>
        </div>
        <div
          className="container"
          style={ReactDOM.Style.make(
            ~order="2",
            ~flexGrow="1",
            ~overflow="hidden",
            ~display="flex",
            ~flexDirection="row",
            (),
          )}>
          <ReactD3Graph.Graph
            id={"model-graph"}
            data={focused
            ->Option.flatMap(focused =>
              state
              ->State.construction(focused)
              ->Option.map(construction => construction->State.Construction.graph->GraphState.data)
            )
            ->Option.getWithDefault(GraphState.empty->GraphState.data)}
            config
            selection={selection->GraphState.Selection.toReactD3Selection}
            onSelectionChange={selectionChange}
            onNodePositionChange={movedNode}
            keybindings={keybindings}
            showGrid
            style={ReactDOM.Style.make(~flexGrow="1", ())}
          />
          <Inspector
            id={"node_inspector"}
            onChange=inspectorChange
            data={focused
            ->Option.flatMap(focused =>
              state
              ->State.construction(focused)
              ->Option.flatMap(construction =>
                switch (
                  GraphState.Selection.nodes(selection),
                  GraphState.Selection.links(selection),
                ) {
                | ([node], []) =>
                  switch construction->State.Construction.getNode(node) {
                  | Some(#token(data)) => {
                      let space =
                        construction
                        ->State.Construction.space
                        ->Option.flatMap(State.getSpace(state, _))
                      Inspector.Data.Token(
                        data,
                        space
                        ->Option.flatMap(space => {
                          let typeSystemName = CSpace.conSpecTypeSystem(space)
                          state
                          ->State.typeSystems
                          ->String.Map.get(typeSystemName)
                          ->Option.map(pts =>
                            pts
                            ->FiniteSet.toArray
                            ->Array.map(p => (p->Type.PrincipalType.type_->Type.name, p))
                            ->String.Map.fromArray
                          )
                        })
                        ->Option.getWithDefault(String.Map.empty),
                        space->Option.map(CSpace.conSpecTypeSystem)->Option.getWithDefault(""),
                        node,
                      )->Some
                    }
                  | Some(#constructor(data)) =>
                    Inspector.Data.Constructor(
                      data,
                      construction
                      ->State.Construction.space
                      ->Option.flatMap(State.getSpace(state, _))
                      ->Option.map(space =>
                        space
                        ->CSpace.conSpecConstructors
                        ->FiniteSet.toArray
                        ->Array.map(c => (CSpace.constructorName(c), c))
                        ->String.Map.fromArray
                      )
                      ->Option.getWithDefault(String.Map.empty),
                      node,
                    )->Some
                  | _ => None
                  }
                | ([], [link]) =>
                  construction
                  ->State.Construction.getLink(link)
                  ->Option.map(data => Inspector.Data.Edge(data, link))
                | ([], []) => Inspector.Data.Construction(construction, focused)->Some
                | _ => Inspector.Data.Multiple->Some
                }
              )
            )
            ->Option.getWithDefault(Inspector.Data.Nothing)}
            nodeIds={selection->GraphState.Selection.nodes}
          />
        </div>
      </div>
    </main>
  }
}

type root
@module("react-dom/client")
external createRoot: Dom.element => root = "createRoot"
@send external render: (root, React.element) => unit = "render"

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => App.init->Promise.thenResolve(init => createRoot(e)->render(<App init />))->ignore
}
