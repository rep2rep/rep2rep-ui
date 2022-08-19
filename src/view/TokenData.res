type t = {
  label: string,
  payload: option<string>,
  type_: option<Type.typ>,
  subtype: option<string>,
  notes: string,
}

module Stable = {
  module V1 = {
    type t = t = {
      label: string,
      payload: option<string>,
      type_: option<Type.typ>,
      subtype: option<string>,
      notes: string,
    }

    let toJson = t =>
      Js.Dict.fromArray([
        ("version", 1->Int.toJson),
        ("label", t.label->String.toJson),
        ("payload", t.payload->Option.toJson(String.toJson)),
        ("type_", t.type_->Option.toJson(Type.typ_toJson)),
        ("subtype", t.subtype->Option.toJson(String.toJson)),
        ("notes", t.notes->String.toJson),
      ])->Js.Json.object_

    let fromJson = json =>
      json
      ->Js.Json.decodeObject
      ->Or_error.fromOption_s("Failed to decode TokenData object JSON")
      ->Or_error.flatMap(dict => {
        let getValue = (key, reader) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(reader)
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(1) => {
            let label = getValue("label", String.fromJson)
            let payload = getValue("payload", Option.fromJson(_, String.fromJson))
            let type_ = getValue("type_", Option.fromJson(_, Type.typ_fromJson))
            let subtype = getValue("subtype", Option.fromJson(_, String.fromJson))
            let notes = getValue("notes", String.fromJson)
            (label, payload, type_, subtype, notes)
            ->Or_error.both5
            ->Or_error.map(((label, payload, type_, subtype, notes)) => {
              label: label,
              payload: payload,
              type_: type_,
              subtype: subtype,
              notes: notes,
            })
          }
        | Or_error.Ok(v) => Or_error.error_ss(["Unknown TokenData version ", Int.toString(v)])
        | Or_error.Err(e) => Or_error.error(e)
        }
      })
  }
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

let hash: t => Hash.t = Hash.record5(
  ("label", String.hash),
  ("payload", Option.hash(_, String.hash)),
  ("type_", ty => ty->Option.map(Type.name)->Option.hash(String.hash)),
  ("subtype", Option.hash(_, String.hash)),
  ("notes", String.hash),
)

// Perhaps we can dispatch these off to a "plugin"? E.g, if we know the RS, we send [t] to
// a plugin which has said "I am a renderer for this RS", and get back a React component and a size.
let size = t =>
  {"width": String.length(t.label)->(f => f * 10)->Int.toFloat->Float.max(20.), "height": 20.}
let render = t =>
  <text x={"50%"} y={"50%"} textAnchor="middle" dominantBaseline="central">
    {React.string(t.label)}
  </text>
