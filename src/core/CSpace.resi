type ctyp = (array<Type.typ>, Type.typ)
type constructor
type token
type configurator
type conSpec

let ctyp_rpc: Rpc.Datatype.t<ctyp>
let constructor_rpc: Rpc.Datatype.t<constructor>
let token_rpc: Rpc.Datatype.t<token>
let configurator_rpc: Rpc.Datatype.t<configurator>
let conSpec_rpc: Rpc.Datatype.t<conSpec>

let constructorName: constructor => string
let constructorSignature: constructor => ctyp

let conSpecName: conSpec => string
let conSpecTypeSystem: conSpec => string
let conSpecConstructors: conSpec => FiniteSet.t<constructor>
