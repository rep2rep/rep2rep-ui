module Kind = {
  type t = Error | Information
  let t_rpc = {
    let name = "Diagnostic.Kind.t"
    let underlying = Rpc.Datatype.either2_(Rpc.Datatype.unit_, Rpc.Datatype.unit_)
    let fromEither = e =>
      switch e {
      | Rpc.Datatype.Either2.FST() => Error
      | Rpc.Datatype.Either2.SND() => Information
      }
    let toEither = t =>
      switch t {
      | Error => Rpc.Datatype.Either2.FST()
      | Information => Rpc.Datatype.Either2.SND()
      }
    underlying->Rpc.Datatype.convert(name, fromEither, toEither)
  }
}

type t = {
  kind: Kind.t,
  message: string,
  affectedTokens: array<Gid.t>,
}

let t_rpc = {
  let name = "Diagnostic.t"
  let underlying = Rpc.Datatype.tuple3_(Kind.t_rpc, String.t_rpc, Array.t_rpc(String.t_rpc))
  let fromTuple = ((kind, message, token_ids)) => {
    kind: kind,
    message: message,
    affectedTokens: token_ids->Array.map(Gid.fromString),
  }
  let toTuple = t => (t.kind, t.message, t.affectedTokens->Array.map(Gid.toString))
  underlying->Rpc.Datatype.convert(name, fromTuple, toTuple)
}

let create = (kind, message, affectedTokens) => {
  kind: kind,
  message: message,
  affectedTokens: affectedTokens,
}

let kind = t => t.kind
let message = t => t.message
let affectedTokens = t => t.affectedTokens
