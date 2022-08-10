type t = {
  source: Gid.t,
  target: Gid.t,
  payload: option<int>,
  notes: string,
}

module Stable = {
  module V1 = {
    type t = t = {
      source: Gid.t,
      target: Gid.t,
      payload: option<int>,
      notes: string,
    }

    let toJson = t =>
      Js.Dict.fromArray([
        ("version", 1->Int.toJson),
        ("source", t.source->Gid.toJson),
        ("target", t.target->Gid.toJson),
        ("payload", t.payload->Option.toJson(Int.toJson)),
        ("notes", t.notes->String.toJson),
      ])->Js.Json.object_

    let fromJson = json =>
      json
      ->Js.Json.decodeObject
      ->Or_error.fromOption_s("Failed to decode EdgeData object JSON")
      ->Or_error.flatMap(dict => {
        let getValue = (key, reader) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(reader)
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(1) => {
            let source = getValue("source", Gid.fromJson)
            let target = getValue("target", Gid.fromJson)
            let payload = getValue("payload", Option.fromJson(_, Int.fromJson))
            let notes = getValue("notes", String.fromJson)
            (source, target, payload, notes)
            ->Or_error.both4
            ->Or_error.map(((source, target, payload, notes)) => {
              source: source,
              target: target,
              payload: payload,
              notes: notes,
            })
          }
        | Or_error.Ok(v) => Or_error.error_ss(["Unknown ConstructorData version ", Int.toString(v)])
        | Or_error.Err(e) => Or_error.error(e)
        }
      })
  }
}

let create = (source, target, payload) => {
  source: source,
  target: target,
  payload: payload,
  notes: "",
}

let duplicate = t => {
  source: t.source,
  target: t.target,
  payload: t.payload,
  notes: t.notes,
}

let source = t => t.source
let target = t => t.target
let payload = t => t.payload
let notes = t => t.notes

let setSource = (t, source) => {...t, source: source}
let setTarget = (t, target) => {...t, target: target}
let setPayload = (t, payload) => {...t, payload: payload}
let setNotes = (t, notes) => {...t, notes: notes}
