type t = bool

let t_rpc: Rpc.Datatype.t<bool>

let not: t => t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>
