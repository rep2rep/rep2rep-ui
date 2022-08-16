type ctyp = (array<Type.typ>, Type.typ)
type constructor = (string, ctyp)
type token = (string, Type.typ)
type configurator = (string, constructor)
type conSpec = {
  name: string,
  typeSystem: string,
  constructors: FiniteSet.t<constructor>,
}

let pair_toJson = ((a, b), a_toJson, b_toJson) => [a->a_toJson, b->b_toJson]->Array.toJson(x => x)
let pair_fromJson = (json, a_fromJson, b_fromJson) =>
  json
  ->Array.fromJson(x => Or_error.create(x))
  ->Or_error.flatMap(arr =>
    switch arr {
    | [a_j, b_j] => {
        let a = a_j->a_fromJson
        let b = b_j->b_fromJson
        (a, b)->Or_error.both
      }
    | _ => Or_error.error_s("Tuple of wrong length in pair json")
    }
  )

let ctyp_rpc =
  Rpc.Datatype.tuple2_(Array.t_rpc(Type.typ_rpc), Type.typ_rpc)->Rpc.Datatype.alias("CSpace.ctyp")
let ctyp_toJson = pair_toJson(_, Array.toJson(_, Type.typ_toJson), Type.typ_toJson)
let ctyp_fromJson = pair_fromJson(_, Array.fromJson(_, Type.typ_fromJson), Type.typ_fromJson)

let constructor_rpc =
  Rpc.Datatype.tuple2_(String.t_rpc, ctyp_rpc)->Rpc.Datatype.alias("CSpace.constructor")
let constructor_toJson = pair_toJson(_, String.toJson, ctyp_toJson)
let constructor_fromJson = pair_fromJson(_, String.fromJson, ctyp_fromJson)

let token_rpc = Rpc.Datatype.tuple2_(String.t_rpc, Type.typ_rpc)->Rpc.Datatype.alias("CSpace.token")

let configurator_rpc =
  Rpc.Datatype.tuple2_(String.t_rpc, constructor_rpc)->Rpc.Datatype.alias("CSpace.configurator")

let conSpec_rpc = Rpc.Datatype.tuple3_(
  String.t_rpc,
  String.t_rpc,
  FiniteSet.t_rpc(constructor_rpc),
)->Rpc.Datatype.convert(
  "CSpace.conSpec",
  ((name, typeSystem, constructors)) => {
    name: name,
    typeSystem: typeSystem,
    constructors: constructors,
  },
  ({name, typeSystem, constructors}) => (name, typeSystem, constructors),
)
let conSpec_toJson = c =>
  Js.Dict.fromArray([
    ("name", c.name->String.toJson),
    ("typeSystem", c.typeSystem->String.toJson),
    ("constructors", c.constructors->FiniteSet.toJson(constructor_toJson)),
  ])->Js.Json.object_
let conSpec_fromJson = json =>
  json
  ->Js.Json.decodeObject
  ->Or_error.fromOption_s("Failed to decode conSpec object JSON")
  ->Or_error.flatMap(dict => {
    let getValue = (key, reader) =>
      dict
      ->Js.Dict.get(key)
      ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
      ->Or_error.flatMap(reader)
    let name = getValue("name", String.fromJson)
    let typeSystem = getValue("typeSystem", String.fromJson)
    let constructors = getValue("constructors", FiniteSet.fromJson(_, constructor_fromJson))
    (name, typeSystem, constructors)
    ->Or_error.both3
    ->Or_error.map(((name, typeSystem, constructors)) => {
      name: name,
      typeSystem: typeSystem,
      constructors: constructors,
    })
  })

let constructorName = ((name, _)) => name
let constructorSignature = ((_, sig)) => sig

let conSpecName = t => t.name
let conSpecTypeSystem = t => t.typeSystem
let conSpecConstructors = t => t.constructors

let tokenFromTokenData = (id, td: TokenData.t) =>
  Option.first([td.subtype->Option.map(Type.fromString), td.type_])
  ->Or_error.fromOption_ss(["Token has no type! ", Gid.toString(id)])
  ->Or_error.map(ty => (Gid.toString(id), ty))
