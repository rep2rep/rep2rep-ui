type typ = string

let typ_rpc = String.t_rpc->Rpc.Datatype.alias("Type.typ")
let typ_toJson = String.toJson
let typ_fromJson = String.fromJson

let fromString = t => t
let name = t => t

let any = ""
let equal = (t, t') => t == t'
let join = ts => ts->Array.joinWith(":")

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
