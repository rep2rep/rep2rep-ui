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
  let make = (~name=?, ~value=?, ~onChange=?, ~style=ReactDOM.Style.make()) => {
    <input
      type_="text"
      ?name
      ?value
      ?onChange
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
  ) => {
    <select
      name
      value={current->Option.map(toString)->Option.getWithDefault("-")}
      onChange={e => onChange(fromString(ReactEvent.Form.target(e)["value"]))}>
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
    let onChange = e => {
      Js.Console.log(e)
      onChange(e)
    }
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
  let make = (~data: TokenData.t, ~onChange) => {
    <>
      <Row>
        <Label> {React.string("Label")} </Label>
        <Input
          value={data.label}
          onChange={e => ReactEvent.Form.target(e)["value"]->Event.Token.Label->onChange}
        />
      </Row>
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

module Construction = {
  @react.component
  let make = (~data: State.Construction.t, ~spaces, ~onChange) => {
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
      <Row>
        <Label> {React.string("Space")} </Label>
        <Selector
          name="space-selector"
          options={String.Map.valuesToArray(spaces)}
          current={data->State.Construction.space}
          toString={space => space.CSpace.name}
          fromString={spaceName => spaces->String.Map.get(spaceName)}
          onChange={e => Event.Construction.SetSpace(e)->onChange}
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
    | Token(TokenData.t, Gid.t)
    | Constructor(ConstructorData.t, String.Map.t<CSpace.constructor>, Gid.t)
    | Construction(State.Construction.t, String.Map.t<CSpace.conSpec>, Gid.t)
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
    | Data.Construction(data, spaces, constructionId) => <Construction data spaces onChange />
    | Data.Token(data, nodeId) =>
      <Token data onChange={e => Event.Construction.UpdateToken(nodeId, e)->onChange} />
    | Data.Constructor(data, constructors, nodeId) =>
      <Constructor
        data constructors onChange={e => Event.Construction.UpdateConstructor(nodeId, e)->onChange}
      />
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
