type typ
let typ_rpc: Rpc.Datatype.t<typ>

let fromString: string => typ
let name: typ => string

let any: typ
let equal: (typ, typ) => bool

module PrincipalType: {
  type t
  let t_rpc: Rpc.Datatype.t<t>

  let type_: t => typ
  let isSubTypeable: t => bool
}
