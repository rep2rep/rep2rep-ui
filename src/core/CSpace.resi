type ctyp = (array<Type.typ>, Type.typ)
type constructor
type token
type configurator
type conSpec

let ctyp_rpc: Rpc.Datatype.t<ctyp>
let ctyp_toJson: ctyp => Js.Json.t
let ctyp_fromJson: Js.Json.t => Or_error.t<ctyp>

let constructor_rpc: Rpc.Datatype.t<constructor>
let constructor_toJson: constructor => Js.Json.t
let constructor_fromJson: Js.Json.t => Or_error.t<constructor>

let token_rpc: Rpc.Datatype.t<token>
let configurator_rpc: Rpc.Datatype.t<configurator>

let conSpec_rpc: Rpc.Datatype.t<conSpec>
let conSpec_toJson: conSpec => Js.Json.t
let conSpec_fromJson: Js.Json.t => Or_error.t<conSpec>

let tokenName: token => string
let tokenType: token => Type.typ

let constructorName: constructor => string
let constructorSignature: constructor => ctyp

let conSpecName: conSpec => string
let conSpecTypeSystem: conSpec => string
let conSpecConstructors: conSpec => FiniteSet.t<constructor>

let tokenFromTokenData: (Gid.t, TokenData.t) => Or_error.t<token>
