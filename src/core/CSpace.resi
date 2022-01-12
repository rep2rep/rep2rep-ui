type ctyp = (array<Type.typ>, Type.typ)
type constructor
type token
type configurator
type conSpec = {
  name: string,
  typeSystem: string,
  constructors: FiniteSet.t<constructor>,
}

let ctyp_rpc: Rpc.Datatype.t<ctyp>
let constructor_rpc: Rpc.Datatype.t<constructor>
let token_rpc: Rpc.Datatype.t<token>
let configurator_rpc: Rpc.Datatype.t<configurator>
let conSpec_rpc: Rpc.Datatype.t<conSpec>

let makeCTyp: (array<Type.typ>, Type.typ) => ctyp
let makeConstructor: (string, ctyp) => constructor
let makeToken: (string, Type.typ) => token
let makeConfigurator: (string, constructor) => configurator
let makeConSpec: (
  ~name: string,
  ~typeSystem: string,
  ~constructors: FiniteSet.t<constructor>,
) => conSpec
