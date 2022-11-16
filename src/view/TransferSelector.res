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
      onChange={e => {
        ReactEvent.Form.stopPropagation(e)
        onChange(fromString(ReactEvent.Form.target(e)["value"]))
      }}
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

@react.component
let make = (~options, ~onTransfer, ~closeModal) => {
  let (target, setTarget) = React.useState(_ => None)
  let (inter, setInter) = React.useState(_ => None)

  let style = ReactDOM.Style.make(~padding="1rem", ~fontSize="0.9rem", ())

  <div style>
    <h4> {React.string("Transfer structure graph")} </h4>
    <span style={ReactDOM.Style.make(~display="block", ~margin="1rem", ())} />
    {React.string("Target space: ")}
    <span style={ReactDOM.Style.make(~margin="0 0.5rem", ())} />
    <Selector
      name="target"
      options={String.Map.keysToArray(options)}
      current={target}
      toString={s => s}
      fromString={s =>
        if s === "-" {
          None
        } else {
          Some(s)
        }}
      onChange={v => setTarget(_ => v)}
    />
    <span style={ReactDOM.Style.make(~display="block", ~margin="0.25rem", ())} />
    {React.string("Via interspace: ")}
    <span style={ReactDOM.Style.make(~margin="0 0.23rem", ())} />
    <Selector
      name="inter"
      options={target
      ->Option.flatMap(target => options->String.Map.get(target))
      ->Option.getWithDefault([])}
      current={inter}
      toString={s => s}
      fromString={s =>
        if s === "-" {
          None
        } else {
          Some(s)
        }}
      onChange={v => setInter(_ => v)}
      enabled={Option.isSome(target)}
    />
    <span style={ReactDOM.Style.make(~display="block", ~margin="1rem", ())} />
    <Button
      onClick={e => {
        ReactEvent.Mouse.preventDefault(e)
        ReactEvent.Mouse.stopPropagation(e)
        onTransfer(Option.both((target, inter)))
        closeModal()
      }}
      value="Transfer"
      enabled={Option.both((target, inter))->Option.isSome}
    />
    {React.string(" ")}
    <Button onClick={_ => closeModal()} value="Cancel" />
  </div>
}
