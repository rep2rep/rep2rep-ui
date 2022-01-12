type t<'a>

let t_rpc: Rpc.Datatype.t<'a> => Rpc.Datatype.t<t<'a>>

let empty: unit => t<'a>
