include module type of Belt.Float

let t_rpc: Rpc.Datatype.t<float>

let toJson: float => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<float>

let max: (int, int) => int
let min: (int, int) => int
