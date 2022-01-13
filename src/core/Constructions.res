type tc = {
  token: CSpace.token,
  constructor: CSpace.constructor,
}
type rec construction =
  | TCPair(tc, array<construction>)
  | Source(CSpace.token)
  | Reference(CSpace.token)
type vertex =
  | Token(CSpace.token)
  | Constructor(CSpace.constructor)
type walk = array<vertex>

let tc_rpc =
  Rpc.Datatype.tuple2_(CSpace.token_rpc, CSpace.constructor_rpc)->Rpc.Datatype.convert(
    "Constructions.tc",
    ((token, constructor)) => {token: token, constructor: constructor},
    ({token, constructor}) => (token, constructor),
  )

let rec construction_rpc_ = () =>
  Rpc.Datatype.either3_(
    Rpc.Datatype.tuple2_(tc_rpc, Array.t_rpc(Rpc.Datatype.recur(construction_rpc_))),
    CSpace.token_rpc,
    CSpace.token_rpc,
  )->Rpc.Datatype.convert(
    "Constructions.construction",
    e =>
      switch e {
      | Rpc.Datatype.Either3.FST((tc, cs)) => TCPair(tc, cs)
      | Rpc.Datatype.Either3.SND(t) => Source(t)
      | Rpc.Datatype.Either3.THD(t) => Reference(t)
      },
    c =>
      switch c {
      | TCPair(tc, cs) => Rpc.Datatype.Either3.FST((tc, cs))
      | Source(t) => Rpc.Datatype.Either3.SND(t)
      | Reference(t) => Rpc.Datatype.Either3.THD(t)
      },
  )
let construction_rpc = construction_rpc_()

let vertex_rpc = Rpc.Datatype.either2_(
  CSpace.token_rpc,
  CSpace.constructor_rpc,
)->Rpc.Datatype.convert(
  "Constructions.vertex",
  e =>
    switch e {
    | Rpc.Datatype.Either2.FST(t) => Token(t)
    | Rpc.Datatype.Either2.SND(c) => Constructor(c)
    },
  v =>
    switch v {
    | Token(t) => Rpc.Datatype.Either2.FST(t)
    | Constructor(c) => Rpc.Datatype.Either2.SND(c)
    },
)

let walk_rpc = Array.t_rpc(vertex_rpc)->Rpc.Datatype.alias("Constructions.walk")

let url = s => "core.construction." ++ s

let size = Rpc_service.require(url("size"), construction_rpc, Int.t_rpc)
let leavesOfConstruction = Rpc_service.require(
  url("leavesOfConstruction"),
  construction_rpc,
  Array.t_rpc(CSpace.token_rpc),
)
let fullTokenSequence = Rpc_service.require(
  url("fullTokenSequence"),
  construction_rpc,
  Array.t_rpc(CSpace.token_rpc),
)
