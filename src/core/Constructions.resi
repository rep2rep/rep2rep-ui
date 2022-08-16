type tc = {
  token: CSpace.token,
  constructor: CSpace.constructor,
}
type rec construction =
  | TCPair(tc, array<construction>)
  | Source(CSpace.token)
  | Reference(CSpace.token)
type walk

let tc_rpc: Rpc.Datatype.t<tc>
let construction_rpc: Rpc.Datatype.t<construction>
let walk_rpc: Rpc.Datatype.t<walk>

let fromViewConstruction: State.Construction.t => Or_error.t<array<construction>>

let size: construction => Rpc.Response.t<int>
let leavesOfConstruction: construction => Rpc.Response.t<array<CSpace.token>>
let fullTokenSequence: construction => Rpc.Response.t<array<CSpace.token>>
