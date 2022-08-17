type typ
let typ_rpc: Rpc.Datatype.t<typ>
let typ_toJson: typ => Js.Json.t
let typ_fromJson: Js.Json.t => Or_error.t<typ>

let fromString: string => typ
let name: typ => string

let any: typ
let equal: (typ, typ) => bool
let join: array<typ> => typ

module PrincipalType: {
  type t
  let t_rpc: Rpc.Datatype.t<t>

  let type_: t => typ
  let isSubTypeable: t => bool
}
