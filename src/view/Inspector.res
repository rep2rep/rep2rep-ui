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

module Token = {
  @react.component
  let make = (~data: TokenData.t, ~principalTypes, ~onChange) => {
    let prinType =
      data.type_->Option.map(Type.name)->Option.flatMap(t => principalTypes->String.Map.get(t))
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
            onChange={e => ReactEvent.Form.target(e)["value"]->Event.Token.Subtype->onChange}
            exclude={%re("/[^a-zA-Z_]/gi")}
          />
        </Row>
      } else {
        React.null
      }}
      <Notes
        name="token-notes"
        value={data.notes}
        onChange={e => ReactEvent.Form.target(e)["value"]->Event.Token.Notes->onChange}
      />
    </>
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
    | Token(TokenData.t, String.Map.t<Type.PrincipalType.t>, Gid.t)
    | Constructor(ConstructorData.t, String.Map.t<CSpace.constructor>, Gid.t)
    | Edge(EdgeData.t, Gid.t)
    | Construction(State.Construction.t, Gid.t)
    | Multiple
}

@react.component
let make = (~id, ~data, ~nodeIds, ~onChange=?) => {
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
      ~padding="0.5rem 0",
      ~width="350px",
      ~display="flex",
      ~flexDirection="column",
      ~borderLeft="1px solid black",
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
    | Data.Construction(data, constructionId) => <Construction data onChange />
    | Data.Token(data, principalTypes, nodeId) =>
      <Token
        data principalTypes onChange={e => Event.Construction.UpdateToken(nodeId, e)->onChange}
      />
    | Data.Constructor(data, constructors, nodeId) =>
      <Constructor
        data constructors onChange={e => Event.Construction.UpdateConstructor(nodeId, e)->onChange}
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
  </HideablePanel2>
}
