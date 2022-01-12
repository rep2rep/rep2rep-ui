type corr = {
  name: string,
  sourcePattern: Pattern.construction,
  targetPattern: Pattern.construction,
  tokenRels: array<Relation.relationship>,
  constructRel: Relation.relationship,
  pullList: array<(Relation.t, Relation.t, array<CSpace.token>)>,
}

let corr_rpc = Rpc.Datatype.tuple6_(
  String.t_rpc,
  Pattern.construction_rpc,
  Pattern.construction_rpc,
  Array.t_rpc(Relation.relationship_rpc),
  Relation.relationship_rpc,
  Array.t_rpc(Rpc.Datatype.tuple3_(Relation.t_rpc, Relation.t_rpc, Array.t_rpc(CSpace.token_rpc))),
)->Rpc.Datatype.convert(
  "Correspondence.corr",
  ((name, sourcePattern, targetPattern, tokenRels, constructRel, pullList)) => {
    name: name,
    sourcePattern: sourcePattern,
    targetPattern: targetPattern,
    tokenRels: tokenRels,
    constructRel: constructRel,
    pullList: pullList,
  },
  ({name, sourcePattern, targetPattern, tokenRels, constructRel, pullList}) => (
    name,
    sourcePattern,
    targetPattern,
    tokenRels,
    constructRel,
    pullList,
  ),
)
