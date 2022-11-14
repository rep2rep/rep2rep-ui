module Row = {
  @react.component
  let make = (~style=ReactDOM.Style.make(), ~children) => {
    <div
      style={ReactDOM.Style.make(
        ~margin="0.125rem 0.5rem",
        ~minHeight="20px",
        ~display="flex",
        ~alignItems="center",
        (),
      )->ReactDOM.Style.combine(style)}
      className="inspector-row">
      {children}
    </div>
  }
}

module Label = {
  @react.component
  let make = (~htmlFor=?, ~help as title=?, ~style=ReactDOM.Style.make(), ~children) => {
    <label
      style={ReactDOM.Style.make(
        ~fontSize="small",
        ~marginRight="0.5rem",
        (),
      )->ReactDOM.Style.combine(style)}
      ?htmlFor
      ?title>
      {children}
    </label>
  }
}

module Input = {
  @react.component
  let make = (~name=?, ~value=?, ~onChange=?, ~exclude=?, ~style=ReactDOM.Style.make()) => {
    let onKeyPress = e => {
      exclude->Option.iter(Js.Re.setLastIndex(_, 0))
      let key = ReactEvent.Keyboard.key(e)
      let shouldIgnore = exclude->Option.map(Js.Re.test_(_, key))->Option.getWithDefault(false)
      if shouldIgnore {
        e->ReactEvent.Keyboard.preventDefault
      }
    }
    <input
      type_="text"
      ?name
      ?value
      ?onChange
      onKeyPress
      style={ReactDOM.Style.make(
        ~flexGrow="1",
        ~border="1px solid #777",
        ~borderRadius="2px",
        ~padding="0.125rem 0.25rem",
        (),
      )->ReactDOM.Style.combine(style)}
    />
  }
}

module Selector = {
  @react.component
  let make = (
    ~name: string,
    ~options: array<'a>,
    ~current: option<'a>,
    ~toString: 'a => string,
    ~fromString: string => option<'a>,
    ~onChange: option<'a> => unit,
    ~enabled: bool=true,
  ) => {
    <select
      name
      value={current->Option.map(toString)->Option.getWithDefault("-")}
      onChange={e => onChange(fromString(ReactEvent.Form.target(e)["value"]))}
      disabled={!enabled}>
      <option value="-" key={name ++ "-option-none"}> {React.string("-")} </option>
      {options
      ->Array.map(a =>
        <option value={toString(a)} key={name ++ "-option-" ++ toString(a)}>
          {React.string(toString(a))}
        </option>
      )
      ->React.array}
    </select>
  }
}

module AttributesEditor = {
  @react.component
  let make = (~name, ~value, ~onChange) => {
    let add = _ => value->List.concat(List.singleton(""))->onChange
    let edit = (e, i) =>
      value
      ->List.mapWithIndex((idx, v) =>
        if idx === i {
          ReactEvent.Form.target(e)["value"]
        } else {
          v
        }
      )
      ->onChange
    let remove = i => value->List.keepWithIndex((_, idx) => idx !== i)->onChange
    <div style={ReactDOM.Style.make(~display="inline-block", ())}>
      {value
      ->List.toArray
      ->Array.mapWithIndex((idx, attr) => {
        <span
          key={name ++ "-" ++ Int.toString(idx)} style={ReactDOM.Style.make(~display="block", ())}>
          <Input
            value={attr}
            onChange={e => edit(e, idx)}
            style={ReactDOM.Style.make(~marginBottom="0.25rem", ~marginRight="0.5rem", ())}
          />
          <input type_="button" value="Delete" onClick={_ => remove(idx)} />
        </span>
      })
      ->React.array}
      <input type_="button" value="Add Attribute" onClick=add />
    </div>
  }
}

module Notes = {
  @react.component
  let make = (~name, ~value=?, ~onChange=?, ~help=?) => {
    let help = help->Option.getWithDefault("Add any other comments about this schema here.")
    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~flexDirection="column",
        ~margin="0.125rem 0.5rem",
        (),
      )}>
      <Label htmlFor={name} help> {React.string("Notes")} </Label>
      <textarea
        name
        ?onChange
        ?value
        style={ReactDOM.Style.make(
          ~height="200px",
          ~border="1px solid #777",
          ~borderRadius="2px",
          ~padding="0.25rem",
          ~marginTop="0.125rem",
          ~fontSize="small",
          ~fontFamily="sans-serif",
          (),
        )}
      />
    </div>
  }
}

module TypeContext = {
  let toposort = arrows => {
    let subTypes =
      arrows->Array.reduce(String.Set.empty, (set, (a, _)) => set->String.Set.add(Type.name(a)))
    let supTypes =
      arrows->Array.reduce(String.Set.empty, (set, (_, b)) => set->String.Set.add(Type.name(b)))
    // Any subtype that is also a subtype has incoming arrows, so it's not a leaf.
    let leaves = String.Set.diff(subTypes, supTypes)->String.Set.toArray->Array.map(Type.fromString)
    let arrows = ref(arrows)
    let result = []
    while leaves != [] {
      let focus = leaves->Js.Array2.shift->Option.getExn
      result->Js.Array2.push(focus)->ignore
      let (outgoing, other) = arrows.contents->Array.partition(((sub, _)) => Type.equal(sub, focus))
      arrows := other
      outgoing->Array.forEach(((_, target)) =>
        if other->Array.some(((_, sup)) => Type.equal(target, sup))->not {
          leaves->Js.Array2.push(target)->ignore
        }
      )
    }
    result
  }

  let groupByLayers = (types, arrows) => {
    let types = types->Array.map(Type.name)
    let arrows = arrows->Array.map(((a, b)) => (Type.name(a), Type.name(b)))
    let typeLayers = Belt.MutableMap.String.make()
    // We know the 'layers' of the diagram by applying a simple algorithm
    // 0 is the base layer, and is initially given to all types
    types->Array.forEach(ty => typeLayers->Belt.MutableMap.String.set(ty, 0))
    types->Array.forEach(ty => {
      // For the i-th element in the toposorted types, we find all outgoing arrows
      let targets = arrows->Array.keepMap(((a, b)) =>
        if a == ty {
          Some(b)
        } else {
          None
        }
      )
      // Assume the ith type has layer l, and the layer of the target type is m.
      let current_layer = typeLayers->Belt.MutableMap.String.getExn(ty)
      targets->Array.forEach(tar => {
        typeLayers->Belt.MutableMap.String.update(
          tar,
          Option.map(
            _,
            // If the m <= l, m := l + 1
            layer =>
              if layer <= current_layer {
                current_layer + 1
              } else {
                layer
              },
          ),
        )
      })
    })
    // Then we invert the map, so we assign layers to types, not types to layers;
    // because the keys are 0-k, we can use an array.
    let n_layers = 1 + typeLayers->Belt.MutableMap.String.valuesToArray->Array.reduce(0, Int.max)
    let layers = Array.make(n_layers, None)->Array.map(_ => [])
    typeLayers->Belt.MutableMap.String.forEach((ty, l) =>
      layers[l]->Option.getExn->Js.Array2.push(Type.fromString(ty))->ignore
    )
    layers
  }

  let positionTypeLabels = layers => {
    let n_layers = layers->Array.length
    let margin = 10.
    let v_padding = 25.
    let layer_height = 12.
    let h_padding = 15.
    let em_factor = 10.
    let pos =
      layers
      ->Array.flatMapWithIndex((i, layer) => {
        let y = Int.toFloat(n_layers - i) *. (layer_height +. v_padding) -. v_padding
        let width =
          layer->Array.reduce(-1. *. h_padding, (tot, ty) =>
            tot +. h_padding +. ty->Type.name->String.approximateEmWidth *. em_factor
          )
        let consumed_width = ref(0.)
        layer->Array.map(ty => {
          let dx = ty->Type.name->String.approximateEmWidth *. em_factor
          let x = consumed_width.contents -. width /. 2. +. dx /. 2.
          consumed_width := consumed_width.contents +. h_padding +. dx
          (ty->Type.name, (x, y, dx))
        })
      })
      ->String.Map.fromArray
    let min_x =
      pos
      ->String.Map.valuesToArray
      ->Array.map(((x, _, dx)) => x -. dx /. 2.)
      ->Array.reduce(0., Float.min)
    let width = min_x *. -2. +. 2. *. margin
    let height = Int.toFloat(n_layers) *. (layer_height +. v_padding) -. v_padding +. 2. *. margin
    (width, height, pos->String.Map.map(((x, y, _)) => (x -. min_x +. margin, y +. margin)))
  }

  let makeLabels = (positions, highlight) => {
    positions
    ->String.Map.toArray
    ->Array.map(((ty, (x, y))) =>
      <text
        key=ty
        textAnchor="middle"
        x={Float.toString(x)}
        y={Float.toString(y)}
        fontSize="12"
        fill={if ty == highlight {
          "black"
        } else {
          "#888"
        }}>
        {React.string(ty)}
      </text>
    )
  }

  let makeArrows = (positions, arrows) => {
    let layer_height = 12.
    let v_offset = 5.
    arrows->Array.mapWithIndex((i, (sub, sup)) => {
      let sub = Type.name(sub)
      let sup = Type.name(sup)
      let (x1, y1) = positions->String.Map.getExn(sub)
      let (x2, y2) = positions->String.Map.getExn(sup)
      let y1 = y1 -. layer_height
      let y2 = y2 +. v_offset
      let (x1, x2) = {
        // For aesthetic reasons, we move the x-coordinates slightly closer together
        // This separates the edges when they meet at a super or sub type
        let scale = 0.8
        let mid = (x1 +. x2) /. 2.
        let f = x => (x -. mid) *. scale +. mid
        (f(x1), f(x2))
      }
      <line
        key={sub ++ "$" ++ sup ++ "$" ++ Int.toString(i)}
        x1={Float.toString(x1)}
        y1={Float.toString(y1)}
        x2={Float.toString(x2)}
        y2={Float.toString(y2)}
        stroke="#888"
        strokeLinecap="round"
        markerEnd="url(#typeArr)"
      />
    })
  }

  let defs = [
    <marker
      key="sigArr1" id="typeArr" markerWidth="6" markerHeight="6" refX="4" refY="3" orient="auto">
      <path
        d="M1 1 L5 3 L1 5" fill="transparent" strokeWidth="1" stroke="#888" strokeLinecap="round"
      />
    </marker>,
  ]

  @react.component
  let make = (~arrows, ~principalType, ~subtype=?) => {
    let principalType = principalType->Type.PrincipalType.type_
    let arrows =
      subtype
      ->Option.map(subtype => arrows->Array.push((Type.fromString(subtype), principalType)))
      ->Option.getWithDefault(arrows)
    let allTypes = if arrows == [] {
      // If there are no arrows, then there's no subtype either!
      [principalType]
    } else {
      toposort(arrows)
    }
    if allTypes->Array.includes(principalType)->not {
      React.null
    } else {
      let layers = groupByLayers(allTypes, arrows)
      let (width, height, positions) = positionTypeLabels(layers)
      let labels = makeLabels(positions, subtype->Option.getWithDefault(principalType->Type.name))
      let arrowMarks = makeArrows(positions, arrows)
      <svg
        width={Float.toString(width)}
        height={Float.toString(height)}
        style={ReactDOM.Style.make(~margin="2rem auto 0 auto", ())}>
        <defs> {React.array(defs)} </defs> {React.array(labels)} {React.array(arrowMarks)}
      </svg>
    }
  }
}

module Token = {
  @react.component
  let make = (~data: TokenData.t, ~principalTypes, ~typeSystem, ~onChange) => {
    let (typeContext, setTypeContext) = React.useState(() => None)
    let shouldRun = ref(true)
    let prinType = React.useMemo2(
      () =>
        data.type_->Option.map(Type.name)->Option.flatMap(t => principalTypes->String.Map.get(t)),
      (data, principalTypes),
    )
    React.useEffect3(() => {
      setTypeContext(_ => None)
      prinType->Option.iter(prinType =>
        prinType
        ->Type.PrincipalType.type_
        ->Type.context(~typeSystem)
        ->Rpc.Response.upon(r =>
          if shouldRun.contents {
            setTypeContext(_ => Some(r))
          }
        )
      )
      Some(() => {shouldRun := false})
    }, (prinType, typeSystem, setTypeContext))
    <>
      <Row>
        <Label> {React.string("Label")} </Label>
        <Input
          value={data.label}
          onChange={e => ReactEvent.Form.target(e)["value"]->Event.Token.Label->onChange}
        />
      </Row>
      <Row>
        <Label> {React.string("Type")} </Label>
        <Selector
          name="token-type-selector"
          current={prinType}
          options={String.Map.valuesToArray(principalTypes)}
          toString={p => p->Type.PrincipalType.type_->Type.name}
          fromString={s => principalTypes->String.Map.get(s)}
          onChange={e => Event.Token.Type(e->Option.map(Type.PrincipalType.type_))->onChange}
        />
      </Row>
      {if prinType->Option.map(Type.PrincipalType.isSubTypeable)->Option.getWithDefault(false) {
        <Row>
          <Label> {React.string("SubType")} </Label>
          <Input
            value={data.subtype->Option.getWithDefault("")}
            onChange={e =>
              ReactEvent.Form.target(e)["value"]
              ->Option.flatMap(v =>
                if v === "" {
                  None
                } else {
                  Some(v)
                }
              )
              ->Event.Token.Subtype
              ->onChange}
            exclude={%re("/[ :,]/gi")}
          />
        </Row>
      } else {
        data.subtype->Option.iter(_ => onChange(Event.Token.Subtype(None)))
        React.null
      }}
      <Notes
        name="token-notes"
        value={data.notes}
        onChange={e => ReactEvent.Form.target(e)["value"]->Event.Token.Notes->onChange}
      />
      {(typeContext, prinType)
      ->Option.both
      ->Option.map(((arrows, principalType)) => {
        let subtype = data.subtype
        <TypeContext arrows principalType ?subtype />
      })
      ->Option.getWithDefault(React.null)}
    </>
  }
}

module ConstructorSpecification = {
  let layoutInputs = intypes => {
    let h_padding = 20.
    let em_factor = 10.
    let intypes = intypes->Array.map(Type.name)
    let width =
      intypes->Array.reduce(-1. *. h_padding, (tot, ty) =>
        tot +. h_padding +. ty->String.approximateEmWidth *. em_factor
      )
    let consumed_width = ref(0.)
    intypes
    ->Array.map(ty => {
      let dx = ty->String.approximateEmWidth *. em_factor
      let x = consumed_width.contents -. width /. 2. +. dx /. 2.
      consumed_width := consumed_width.contents +. h_padding +. dx
      x
    })
    ->(xs => (width, xs))
  }

  let layoutConstructor = name => {
    let em_factor = 10.
    let offset = 10.
    let width = name->String.approximateEmWidth *. em_factor
    (2. *. (width +. offset), offset)
  }

  let makeInputTypeNodes = (width, y, typesWithX) => {
    typesWithX->Array.mapWithIndex((i, (ty, x)) => {
      <text
        key={Type.name(ty) ++ Int.toString(i)}
        textAnchor="middle"
        x={Float.toString(x +. width /. 2.)}
        y={Float.toString(y)}
        fontSize="12"
        fill={"#888"}>
        {React.string(Type.name(ty))}
      </text>
    })
  }

  let makeOutputTypeNode = (width, y, ty) => {
    <text
      textAnchor="middle"
      x={Float.toString(width /. 2.)}
      y={Float.toString(y)}
      fontSize="12"
      fill={"#888"}>
      {React.string(Type.name(ty))}
    </text>
  }

  let makeConstructorNode = (width, y, name, offset) => {
    <>
      <circle
        cx={Float.toString(width /. 2.)}
        cy={Float.toString(y)}
        fill="white"
        r="5"
        stroke="black"
        strokeWidth="1"
      />
      <text
        fontSize="12"
        textAnchor="left"
        x={Float.toString(width /. 2. +. offset)}
        y={Float.toString(y +. 4.)}>
        {React.string(name)}
      </text>
    </>
  }

  let makeArrows = (width, in_xs, in_y, out_y, constructor_y) => {
    let shift_up = 15.
    let shift_down = 5.
    let result = in_xs->Array.mapWithIndex((i, x) => {
      let i = i + 1
      <g key={Float.toString(x)}>
        <path
          d={[
            "M " ++ Float.toString(width /. 2. +. x) ++ " " ++ Float.toString(in_y -. shift_up),
            "L " ++ Float.toString(width /. 2.) ++ " " ++ Float.toString(constructor_y),
          ]->Js.Array2.joinWith(" ")}
          stroke="black"
          strokeLinecap="round"
          fill="transparent"
          markerEnd="url(#sigArr1)"
        />
        <svg
          x={Float.toString((width +. x) /. 2. -. 5.)}
          y={Float.toString((in_y -. shift_up +. constructor_y) /. 2. -. 5.)}>
          <circle r="5" cx="5" cy="5" fill="white" />
          <text textAnchor="middle" x="5" y="8.5" fontSize="9">
            {React.string(Int.toString(i))}
          </text>
        </svg>
      </g>
    })
    result
    ->Js.Array2.push({
      <path
        key="out"
        d={"M " ++
        Float.toString(width /. 2.) ++
        " " ++
        Float.toString(constructor_y) ++
        " L " ++
        Float.toString(width /. 2.) ++
        " " ++
        Float.toString(out_y +. shift_down)}
        stroke="black"
        strokeLinecap="round"
        fill="transparent"
        markerEnd="url(#sigArr2)"
      />
    })
    ->ignore
    result
  }

  let defs = [
    <marker
      key="sigArr1" id="sigArr1" markerWidth="6" markerHeight="6" refX="11" refY="3" orient="auto">
      <path d="M1 1 L5 3 L1 5 Z" fill="black" strokeWidth="1" stroke="#000" />
    </marker>,
    <marker
      key="sigArr2" id="sigArr2" markerWidth="6" markerHeight="6" refX="5" refY="3" orient="auto">
      <path d="M1 1 L5 3 L1 5 Z" fill="black" strokeWidth="1" stroke="#000" />
    </marker>,
  ]

  @react.component
  let make = (~name, ~signature) => {
    let padding = 10.
    let em_factor = 10.
    let out_y = 10. +. padding
    let constructor_y = 50. +. padding
    let in_y = 110. +. padding
    let (intypes, outtype) = signature
    // Calculate widths and x-coordinates assume a zero centre
    let (w1, intypePositions) = layoutInputs(intypes)
    let w2 = outtype->Type.name->String.approximateEmWidth *. em_factor
    let (w3, constructorPosition) = layoutConstructor(name)
    // The needed width is the largest
    let width = [w1, w2, w3]->Array.reduce(0., Float.max) +. 2. *. padding
    // Using the positions and widths, let's layout the types
    let intypeNodes = makeInputTypeNodes(width, in_y, Array.zip(intypes, intypePositions))
    let outtypeNode = makeOutputTypeNode(width, out_y, outtype)
    let constructorNode = makeConstructorNode(width, constructor_y, name, constructorPosition)
    let arrows = makeArrows(width, intypePositions, in_y, out_y, constructor_y)
    <svg
      width={Float.toString(width)}
      height={Float.toString(in_y +. 2. *. padding)}
      style={ReactDOM.Style.make(~margin="2rem auto 0 auto", ())}>
      <defs> {React.array(defs)} </defs>
      {React.array(arrows)}
      {React.array(intypeNodes)}
      {outtypeNode}
      {constructorNode}
    </svg>
  }
}

module Constructor = {
  @react.component
  let make = (~data: ConstructorData.t, ~constructors, ~onChange) => {
    <>
      <Row>
        <Label> {React.string("Name")} </Label>
        <Selector
          name="constructor-selector"
          current={data.constructor}
          options={String.Map.valuesToArray(constructors)}
          toString={c => CSpace.constructorName(c)}
          fromString={s => constructors->String.Map.get(s)}
          onChange={e => Event.Constructor.Constructor(e)->onChange}
        />
      </Row>
      <Notes
        name="constructor-notes"
        value={data.notes}
        onChange={e => ReactEvent.Form.target(e)["value"]->Event.Constructor.Notes->onChange}
      />
      {data.constructor
      ->Option.map(constructor =>
        <ConstructorSpecification
          name={constructor->CSpace.constructorName}
          signature={constructor->CSpace.constructorSignature}
        />
      )
      ->Option.getWithDefault(React.null)}
    </>
  }
}

module Edge = {
  @react.component
  let make = (~data: EdgeData.t, ~onChange) => {
    <>
      <Row>
        <Label> {React.string("Label")} </Label>
        <input
          type_="number"
          min="1"
          value={EdgeData.payload(data)->Option.map(Int.toString)->Option.getWithDefault("")}
          onChange={e =>
            ReactEvent.Form.target(e)["value"]->Int.fromString->Event.Edge.Value->onChange}
        />
      </Row>
      <Notes
        name="edge-notes"
        value={EdgeData.notes(data)}
        onChange={e => ReactEvent.Form.target(e)["value"]->Event.Edge.Notes->onChange}
      />
    </>
  }
}

module Construction = {
  @react.component
  let make = (~data: State.Construction.t, ~onChange) => {
    <>
      <Row>
        <Label> {React.string("Name")} </Label>
        <Input
          value={data->State.Construction.metadata->State.Construction.Metadata.name}
          onChange={e =>
            ReactEvent.Form.target(e)["value"]
            ->Event.Construction.Metadata.Name
            ->Event.Construction.UpdateMetadata
            ->onChange}
        />
      </Row>
      <Notes
        name="construction-notes"
        value={data->State.Construction.metadata->State.Construction.Metadata.notes}
        onChange={e =>
          ReactEvent.Form.target(e)["value"]
          ->Event.Construction.Metadata.Notes
          ->Event.Construction.UpdateMetadata
          ->onChange}
      />
    </>
  }
}

module Data = {
  type t =
    | Nothing
    | Token(TokenData.t, String.Map.t<Type.PrincipalType.t>, string, Gid.t)
    | Constructor(ConstructorData.t, String.Map.t<CSpace.constructor>, Gid.t)
    | Edge(EdgeData.t, Gid.t)
    | Construction(State.Construction.t, Gid.t)
    | Multiple
}

@react.component
let make = (~id, ~data, ~onChange=?) => {
  let onChange = onChange->Option.getWithDefault(_ => ())
  <HideablePanel2
    id
    className="inspector-panel"
    toggle={(~hidden) =>
      <div
        style={ReactDOM.Style.make(
          ~cursor="default",
          ~userSelect="none",
          ~position="absolute",
          ~top="40px",
          ~zIndex="100000",
          ~right={
            if hidden {
              "10px"
            } else {
              "350px"
            }
          },
          ~fontSize="16px",
          (),
        )}>
        {if hidden {
          React.string(Js.String2.fromCharCode(9001))
        } else {
          React.string(Js.String2.fromCharCode(9002))
        }}
      </div>}
    style={ReactDOM.Style.make(
      ~order="2",
      ~width="350px",
      ~display="flex",
      ~flexDirection="column",
      ~borderLeft="1px solid black",
      ~overflow="hidden",
      (),
    )}>
    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~flexGrow="1",
        ~flexDirection="column",
        ~overflowY="auto",
        ~padding="0.5rem 0",
        (),
      )}>
      {switch data {
      | Data.Nothing =>
        <span
          style={ReactDOM.Style.make(
            ~display="block",
            ~marginTop="50%",
            ~color="grey",
            ~fontSize="small",
            ~textAlign="center",
            (),
          )}
          className="inspector-panel-empty-message">
          {React.string("Select a structure graph")}
        </span>
      | Data.Construction(data, _) => <Construction data onChange />
      | Data.Token(data, principalTypes, typeSystem, nodeId) =>
        <Token
          data
          principalTypes
          typeSystem
          onChange={e => Event.Construction.UpdateToken(nodeId, e)->onChange}
        />
      | Data.Constructor(data, constructors, nodeId) =>
        <Constructor
          data
          constructors
          onChange={e => Event.Construction.UpdateConstructor(nodeId, e)->onChange}
        />
      | Data.Edge(data, linkId) =>
        <Edge data onChange={e => Event.Construction.UpdateEdge(linkId, e)->onChange} />
      | Data.Multiple =>
        <span
          style={ReactDOM.Style.make(
            ~display="block",
            ~marginTop="50%",
            ~color="grey",
            ~fontSize="small",
            ~textAlign="center",
            (),
          )}
          className="inspector-panel-multiple-message">
          {React.string("Multiple nodes selected")}
        </span>
      }}
    </div>
  </HideablePanel2>
}
