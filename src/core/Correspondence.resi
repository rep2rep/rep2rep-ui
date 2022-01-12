type corr = {
  name: string,
  sourcePattern: Pattern.construction,
  targetPattern: Pattern.construction,
  tokenRels: array<Relation.relationship>,
  constructRel: Relation.relationship,
  pullList: array<(Relation.t, Relation.t, array<CSpace.token>)>,
}

let corr_rpc: Rpc.Datatype.t<corr>
