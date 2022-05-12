type typ = string

let typ_rpc = String.t_rpc->Rpc.Datatype.alias("Type.typ")

let fromString = t => t
let name = t => t

let any = ""
let equal = (t, t') => t == t'

module PrincipalType = {
  type t = {
    typ: typ,
    subTypeable: bool,
  }
  let t_rpc =
    Rpc.Datatype.tuple2_(typ_rpc, Bool.t_rpc)->Rpc.Datatype.convert(
      "Type.PrincipalType.t",
      ((typ, subTypeable)) => {typ: typ, subTypeable: subTypeable},
      ({typ, subTypeable}) => (typ, subTypeable),
    )

  let type_ = t => t.typ
  let isSubTypeable = t => t.subTypeable
}
