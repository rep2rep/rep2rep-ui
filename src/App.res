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
      ~labelProperty=link =>
        link->ReactD3Graph.Link.payload->Option.map(Int.toString)->Option.getWithDefault(""),
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

    let focused = state->State.focused
    let selection =
      focused
      ->Option.flatMap(state->State.construction(_))
      ->Option.map(State.Construction.graph)
      ->Option.map(GraphState.selection)
      ->Option.getWithDefault(GraphState.Selection.empty)

    let dispatchC = e =>
      focused->Option.iter(focused => dispatch(Event.ConstructionEvent(focused, e)))

    let newConstruction = () => dispatch(Event.NewConstruction(Gid.create(), "Construction"))
    let deleteConstruction = id => dispatch(Event.DeleteConstruction(id))
    let focusConstruction = id => dispatch(Event.FocusConstruction(id))
    let duplicateConstruction = id => dispatch(Event.DuplicateConstruction(id, Gid.create()))
    let renameConstruction = (id, newName) =>
      dispatch(Event.ConstructionEvent(id, Event.Construction.Rename(newName)))
    let reorderConstructions = newOrder => dispatch(Event.ReorderConstructions(newOrder))
    let importConstruction = _ => Js.Console.log("Import construction")
    let exportConstruction = id => Js.Console.log("Export construction " ++ Gid.toString(id))

    let canUndo = focused->Option.map(State.canUndo(state))->Option.getWithDefault(false)
    let canRedo = focused->Option.map(State.canRedo(state))->Option.getWithDefault(false)
    let undo = _ => focused->Option.iter(focused => dispatch(Event.Undo(focused)))
    let redo = _ => focused->Option.iter(focused => dispatch(Event.Redo(focused)))

    let toolbarActive = focused->Option.isSome
    let addTokenNodeAt = (_, ~x, ~y) => dispatchC(Event.Construction.AddToken(Gid.create(), x, y))
    let addConstructorNodeAt = (_, ~x, ~y) =>
      dispatchC(Event.Construction.AddConstructor(Gid.create(), x, y))
    let duplicateNodes = _ => Js.Console.log("Duplicate Nodes!")
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
      ("Ctrl+d", (e, ~x as _, ~y as _) => duplicateNodes(e)),
    ])

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
        onImport={importConstruction}
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
          <Button
            onClick={duplicateNodes} value="Duplicate" enabled={toolbarActive} tooltip="Ctrl+D"
          />
          <Button.Separator />
          <Button onClick={connectNodes} value="Connect" enabled={toolbarActive} tooltip="E" />
          <Button.Separator />
          <Button onClick={deleteSelection} value="Delete" enabled={toolbarActive} tooltip="X" />
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
            style={ReactDOM.Style.make(~flexGrow="1", ())}
          />
          // <InspectorPanel
          //   id={"node_inspector"}
          //   onChange=slotsChange
          //   data={focused
          //   ->Option.flatMap(focused =>
          //     state
          //     ->State.model(focused)
          //     ->Option.map(model => {
          //       let slots = model->State.Model.slotsForSelection(selection)->Gid.Map.toArray
          //       switch slots {
          //       | [] => InspectorState.Global(State.Model.info(model))
          //       | [(id, slot)] => InspectorState.Single(id, slot)
          //       | multi => InspectorState.Multiple(multi)
          //       }
          //     })
          //   )
          //   ->Option.getWithDefault(InspectorState.Empty)}
          // />
        </div>
      </div>
    </main>
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
