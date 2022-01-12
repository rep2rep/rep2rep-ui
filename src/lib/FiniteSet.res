type t<'a> = array<'a>

let t_rpc = a_rpc =>
  Array.t_rpc(a_rpc)->Rpc.Datatype.alias("FiniteSet.t<" ++ Rpc.Datatype.name(a_rpc) ++ ">")

let empty = () => []
