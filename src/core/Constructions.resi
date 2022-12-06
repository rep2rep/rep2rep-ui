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

let toOrugaString: construction => Rpc.Response.t<string>
let fromOrugaString: ((string, string)) => Rpc.Response.t<construction>
