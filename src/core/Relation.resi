type t
type relationship = (array<CSpace.token>, array<CSpace.token>, t)

let t_rpc: Rpc.Datatype.t<t>
let relationship_rpc: Rpc.Datatype.t<relationship>

let make: string => t
