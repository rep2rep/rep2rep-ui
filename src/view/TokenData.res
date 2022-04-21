type t = {
  label: string,
  payload: option<string>,
  type_: option<Type.typ>,
  subtype: option<string>,
  notes: string,
}

let create = (~payload=?, ~type_=?, ~subtype=?, ~notes="", label) => {
  label: label,
  payload: payload,
  type_: type_,
  subtype: subtype,
  notes: notes,
}

let duplicate = t => {
  label: t.label,
  payload: t.payload,
  type_: t.type_,
  subtype: t.subtype,
  notes: t.notes,
}

// Perhaps we can dispatch these off to a "plugin"? E.g, if we know the RS, we send [t] to
// a plugin which has said "I am a renderer for this RS", and get back a React component and a size.
let size = t =>
  {"width": String.length(t.label)->(f => f * 10)->Int.toFloat->Float.max(20.), "height": 20.}
let render = t =>
  <text x={"50%"} y={"50%"} textAnchor="middle" dominantBaseline="central">
    {React.string(t.label)}
  </text>
