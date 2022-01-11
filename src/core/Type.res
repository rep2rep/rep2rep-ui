type typ = string

let typ_rpc = String.t_rpc->Rpc.Datatype.alias("Type.typ")

let fromString = t => t
let name = t => t

let any = ""
let equal = (t, t') => t == t'
