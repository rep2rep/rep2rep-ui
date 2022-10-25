type t = {
  constructor: option<CSpace.constructor>,
  notes: string,
}

module Stable = {
  module V1 = {
    type t = t = {
      constructor: option<CSpace.constructor>,
      notes: string,
    }

    let toJson = t =>
      Js.Dict.fromArray([
        ("version", 1->Int.toJson),
        ("constructor", t.constructor->Option.toJson(CSpace.constructor_toJson)),
        ("notes", t.notes->String.toJson),
      ])->Js.Json.object_

    let fromJson = json =>
      json
      ->Js.Json.decodeObject
      ->Or_error.fromOption_s("Failed to decode ConstructorData object JSON")
      ->Or_error.flatMap(dict => {
        let getValue = (key, reader) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(reader)
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(1) => {
            let constructor = getValue(
              "constructor",
              Option.fromJson(_, CSpace.constructor_fromJson),
            )
            let notes = getValue("notes", String.fromJson)
            (constructor, notes)
            ->Or_error.both
            ->Or_error.map(((constructor, notes)) => {
              constructor: constructor,
              notes: notes,
            })
          }
        | Or_error.Ok(v) => Or_error.error_ss(["Unknown ConstructorData version ", Int.toString(v)])
        | Or_error.Err(e) => Or_error.error(e)
        }
      })
  }
}

let create = (~notes="", ()) => {constructor: None, notes: notes}
let duplicate = t => {
  constructor: t.constructor,
  notes: t.notes,
}
let isValid = _ => Result.Ok()

let hash_constructor = con => {
  let name = CSpace.constructorName(con)
  let (ins, out) = CSpace.constructorSignature(con)
  let type_hash = ty => ty->Type.name->String.hash

  let in_hash = ins->Array.hash(type_hash)
  let out_hash = type_hash(out)
  let name_hash = String.hash(name)
  [name_hash, in_hash, out_hash]->Hash.combine
}

let hash = Hash.record2(("constructor", hash_constructor), ("notes", String.hash))
