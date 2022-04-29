module BoolStore = LocalStorage.MakeJsonable(Bool)

module App = {
  // let forAaron: unit => Rpc.Response.t<Constructions.construction> = Rpc_service.require(
  //   "aarons.forAaron",
  //   Rpc.Datatype.unit_,
  //   Constructions.construction_rpc,
  // )

  let init = State.load()->Option.getWithDefault(State.empty)
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
          <path d="M1 1 L7 4 L1 7" fill="none" strokeWidth="1" stroke="#000000" />
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
  let make = () => {
    // forAaron()->Rpc.Response.upon(construction => {
    //   Js.Console.log(construction)

    //   construction->Constructions.size->Rpc.Response.upon(size => Js.Console.log({"size": size}))

    //   construction
    //   ->Constructions.leavesOfConstruction
    //   ->Rpc.Response.upon(leaves => Js.Console.log({"leaves": leaves}))

    //   construction
    //   ->Constructions.fullTokenSequence
    //   ->Rpc.Response.upon(seq => Js.Console.log({"full token sequence": seq}))
    // })

    let (state, dispatch) = React.useReducer(reducer, init)

    React.useEffect0(() => {
      state
      ->State.loadSpaces
      ->Rpc.Response.upon(state => {
        Js.Console.log(state)
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

    let newConstruction = () => dispatch(Event.NewConstruction(Gid.create(), "Structure graph"))
    let deleteConstruction = id => dispatch(Event.DeleteConstruction(id))
    let focusConstruction = id => dispatch(Event.FocusConstruction(id))
    let duplicateConstruction = id => dispatch(Event.DuplicateConstruction(id, Gid.create()))
    let renameConstruction = (id, newName) =>
      dispatch(
        Event.ConstructionEvent(
          id,
          Event.Construction.UpdateMetadata(Event.Construction.Metadata.Name(newName)),
        ),
      )
    let reorderConstructions = newOrder => dispatch(Event.ReorderConstructions(newOrder))
    let importConstructions = _ => Dialog.alert("Importing structure graphs is not yet supported")
    let exportConstruction = id => Dialog.alert("Exporting structure graphs " ++ Gid.toString(id))

    let canUndo = focused->Option.map(State.canUndo(state))->Option.getWithDefault(false)
    let canRedo = focused->Option.map(State.canRedo(state))->Option.getWithDefault(false)
    let undo = _ => focused->Option.iter(focused => dispatch(Event.Undo(focused)))
    let redo = _ => focused->Option.iter(focused => dispatch(Event.Redo(focused)))

    let toolbarActive = focused->Option.isSome
    let addTokenNodeAt = (_, ~x, ~y) => dispatchC(Event.Construction.AddToken(Gid.create(), x, y))
    let addConstructorNodeAt = (_, ~x, ~y) =>
      dispatchC(Event.Construction.AddConstructor(Gid.create(), x, y))
    // let duplicateNodes = _ => Js.Console.log("Duplicate Nodes!")
    let connectNodes = _ =>
      switch GraphState.Selection.nodes(selection) {
      | [self] => dispatchC(Event.Construction.ConnectNodes(Gid.create(), self, self))
      | [source, target] => dispatchC(Event.Construction.ConnectNodes(Gid.create(), source, target))
      | _ => ()
      }
    let deleteSelection = _ => {
      selection
      ->GraphState.Selection.nodes
      ->Array.forEach(nodeId => {
        focused
        ->Option.flatMap(State.construction(state, _))
        ->Option.map(State.Construction.graph)
        ->Option.map(GraphState.incidentLinks(_, ~nodeId))
        ->Option.map(o => Array.concat(o["in"], o["out"]))
        ->Option.iter(Array.forEach(_, linkId => dispatchC(Event.Construction.DeleteLink(linkId))))
        dispatchC(Event.Construction.DeleteNode(nodeId))
      })
      selection
      ->GraphState.Selection.links
      ->Array.forEach(linkId => dispatchC(Event.Construction.DeleteLink(linkId)))
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

    module K = GlobalKeybindings.KeyBinding
    GlobalKeybindings.set([
      K.create("Cmd+z", undo),
      K.create("Cmd+Shift+z", redo),
      K.create("Cmd+y", redo),
    ])

    let keybindings = Js.Dict.fromArray([
      ("t", addTokenNodeAt),
      ("c", addConstructorNodeAt),
      ("e", (e, ~x as _, ~y as _) => connectNodes(e)),
      ("x", (e, ~x as _, ~y as _) => deleteSelection(e)),
      ("Backspace", (e, ~x as _, ~y as _) => deleteSelection(e)),
      ("Delete", (e, ~x as _, ~y as _) => deleteSelection(e)),
      // ("Ctrl+d", (e, ~x as _, ~y as _) => duplicateNodes(e)),
    ])

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
        (),
      )}>
      <FilePanel
        id="file-panel"
        constructions={State.constructions(state)}
        active={State.focused(state)}
        onCreate={newConstruction}
        onDelete={deleteConstruction}
        onSelect={focusConstruction}
        onDuplicate={duplicateConstruction}
        onChangedName={renameConstruction}
        onReorder={reorderConstructions}
        onImport={importConstructions}
        onExport={exportConstruction}
      />
      <div
        className="editor-panel"
        style={ReactDOM.Style.make(
          ~order="2",
          ~flexGrow="1",
          ~display="flex",
          ~flexDirection="column",
          ~height="100%",
          (),
        )}>
        <div
          className="graph-header"
          style={ReactDOM.Style.make(
            ~order="1",
            ~display="flex",
            ~alignItems="center",
            ~height="40px",
            ~borderBottom="1px solid black",
            ~padding="0 0.5rem",
            (),
          )}>
          <Button onClick={undo} value="Undo" enabled={canUndo} tooltip="Cmd+Z" />
          <Button onClick={redo} value="Redo" enabled={canRedo} tooltip="Cmd+Shift+Z" />
          <Button.Separator />
          <Button
            onClick={addTokenNodeAt(_, ~x=0., ~y=0.)}
            value="Token"
            enabled={toolbarActive}
            tooltip="T"
          />
          <Button
            onClick={addConstructorNodeAt(_, ~x=0., ~y=0.)}
            value="Constructor"
            enabled={toolbarActive}
            tooltip="C"
          />
          // <Button
          //   onClick={duplicateNodes} value="Duplicate" enabled={toolbarActive} tooltip="Ctrl+D"
          // />
          <Button.Separator />
          <Button onClick={connectNodes} value="Connect" enabled={toolbarActive} tooltip="E" />
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
          // <Button.Separator />
          // <a href="manual.html" target="_blank"> {React.string("Manual")} </a>
        </div>
        <div
          className="container"
          style={ReactDOM.Style.make(
            ~order="2",
            ~flexGrow="1",
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
                  | Some(#token(data)) => Inspector.Data.Token(data, node)->Some
                  | Some(#constructor(data)) =>
                    Inspector.Data.Constructor(
                      data,
                      construction
                      ->State.Construction.space
                      ->Option.map(space =>
                        space.constructors
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
                | ([], []) =>
                  Inspector.Data.Construction(construction, State.spaces(state), focused)->Some
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

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
