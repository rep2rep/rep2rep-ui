type t = string
type relationship = (array<CSpace.token>, array<CSpace.token>, t)

let t_rpc = String.t_rpc->Rpc.Datatype.alias("Relation.t")
let relationship_rpc =
  Rpc.Datatype.tuple3_(
    Array.t_rpc(CSpace.token_rpc),
    Array.t_rpc(CSpace.token_rpc),
    t_rpc,
  )->Rpc.Datatype.alias("Relation.relationship")

let make = t => t
