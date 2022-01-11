include module type of Js.String2

let t_rpc: Rpc.Datatype.t<string>

let toJson: string => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<string>
