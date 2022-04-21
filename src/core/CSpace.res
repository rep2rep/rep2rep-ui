type ctyp = (array<Type.typ>, Type.typ)
type constructor = (string, ctyp)
type token = (string, Type.typ)
type configurator = (string, constructor)
type conSpec = {
  name: string,
  typeSystem: string,
  constructors: FiniteSet.t<constructor>,
}

let ctyp_rpc =
  Rpc.Datatype.tuple2_(Array.t_rpc(Type.typ_rpc), Type.typ_rpc)->Rpc.Datatype.alias("CSpace.ctyp")

let constructor_rpc =
  Rpc.Datatype.tuple2_(String.t_rpc, ctyp_rpc)->Rpc.Datatype.alias("CSpace.constructor")

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

let makeCTyp = (ts, t) => (ts, t)
let makeConstructor = (s, t) => (s, t)
let makeToken = (s, t) => (s, t)
let makeConfigurator = (s, c) => (s, c)
let makeConSpec = (~name, ~typeSystem, ~constructors) => {
  name: name,
  typeSystem: typeSystem,
  constructors: constructors,
}

let constructorName = ((name, _)) => name
let constructorSignature = ((_, sig)) => sig
