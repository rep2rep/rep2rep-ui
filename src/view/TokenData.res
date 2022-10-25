type t = {
  label: string,
  payload: option<(string, float, float)>,
  type_: option<Type.typ>,
  subtype: option<string>,
  notes: string,
}

module Stable = {
  module V1 = {
    type t = t = {
      label: string,
      payload: option<(string, float, float)>,
      type_: option<Type.typ>,
      subtype: option<string>,
      notes: string,
    }

    let triple_toJson: (
      ('a, 'b, 'c),
      'a => Js.Json.t,
      'b => Js.Json.t,
      'c => Js.Json.t,
    ) => Js.Json.t = ((a, b, c), a_toJson, b_toJson, c_toJson) => {
      [a_toJson(a), b_toJson(b), c_toJson(c)]->Array.toJson(j => j)
    }
    let triple_fromJson: (
      Js.Json.t,
      Js.Json.t => Or_error.t<'a>,
      Js.Json.t => Or_error.t<'b>,
      Js.Json.t => Or_error.t<'c>,
    ) => Or_error.t<('a, 'b, 'c)> = (json, a_fromJson, b_fromJson, c_fromJson) => {
      json
      ->Array.fromJson(j => Or_error.create(j))
      ->Or_error.flatMap(arr =>
        switch arr {
        | [a, b, c] => (a_fromJson(a), b_fromJson(b), c_fromJson(c))->Or_error.both3
        | _ => Or_error.error_s("JSON triple does not have three elements")
        }
      )
    }

    let toJson = t =>
      Js.Dict.fromArray([
        ("version", 1->Int.toJson),
        ("label", t.label->String.toJson),
        (
          "payload",
          t.payload->Option.toJson(triple_toJson(_, String.toJson, Float.toJson, Float.toJson)),
        ),
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
            let payload = getValue(
              "payload",
              Option.fromJson(
                _,
                triple_fromJson(_, String.fromJson, Float.fromJson, Float.fromJson),
              ),
            )
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
  (
    "payload",
    Option.hash(_, ((a, b, c)) => Hash.combine([String.hash(a), Float.hash(b), Float.hash(c)])),
  ),
  ("type_", ty => ty->Option.map(Type.name)->Option.hash(String.hash)),
  ("subtype", Option.hash(_, String.hash)),
  ("notes", String.hash),
)

let isValid = _ => Result.Ok()

let setPayload = (t, payload) => {...t, payload: payload}

let size = t =>
  t.payload
  ->Option.map(((_, w, h)) => {"width": w, "height": h})
  ->Option.getWithDefault({
    "width": String.length(t.label)->(f => f * 10)->Int.toFloat->Float.max(20.),
    "height": 20.,
  })

let render = t =>
  t.payload
  ->Option.map(((payload, w, h)) => {
    <foreignObject width={Float.toString(w)} height={Float.toString(h)}>
      <div xmlns="http://www.w3.org/1999/xhtml" dangerouslySetInnerHTML={"__html": payload} />
    </foreignObject>
  })
  ->Option.getWithDefault(
    <text x={"50%"} y={"50%"} textAnchor="middle" dominantBaseline="central">
      {React.string(t.label)}
    </text>,
  )
