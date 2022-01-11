type t = bool

let t_rpc = Rpc.Datatype.bool_

let not = t => !t

let toJson = Js.Json.boolean
let fromJson = json =>
  json->Js.Json.decodeBoolean->Or_error.fromOption_s("JSON is not a valid boolean")
