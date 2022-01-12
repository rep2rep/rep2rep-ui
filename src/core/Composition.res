type rec composition =
  | Composition({
      construct: CSpace.token,
      attachments: array<(Constructions.construction, array<composition>)>,
    })

let rec composition_rpc_ = () =>
  Rpc.Datatype.tuple2_(
    CSpace.token_rpc,
    Array.t_rpc(
      Rpc.Datatype.tuple2_(
        Constructions.construction_rpc,
        Array.t_rpc(Rpc.Datatype.recur(composition_rpc_)),
      ),
    ),
  )->Rpc.Datatype.convert(
    "Composition.composition",
    ((construct, attachments)) => Composition({construct: construct, attachments: attachments}),
    (Composition({construct, attachments})) => (construct, attachments),
  )
let composition_rpc = composition_rpc_()
