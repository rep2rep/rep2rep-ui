module Kind: {
  type t = Error | Information
  let t_rpc: Rpc.Datatype.t<t>
}

type t

let t_rpc: Rpc.Datatype.t<t>

let create: (Kind.t, string, array<Gid.t>) => t

let kind: t => Kind.t
let message: t => string
let affectedTokens: t => array<Gid.t>
