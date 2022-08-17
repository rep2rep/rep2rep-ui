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

let fromViewConstruction = c => {
  let tokens =
    c
    ->State.Construction.tokens
    ->Array.mapPartial(id => State.Construction.getNode(c, id)->Option.map(t => (id, t)))
    ->Array.mapPartial(((id, n)) =>
      switch n {
      | #token(t) => Some((id, t))
      | _ => None
      }
    )
  let constructors =
    c
    ->State.Construction.constructors
    ->Array.mapPartial(id => State.Construction.getNode(c, id)->Option.map(c' => (id, c')))
    ->Array.mapPartial(((id, n)) =>
      switch n {
      | #constructor(c) => Some((id, c))
      | _ => None
      }
    )
  let links = c->State.Construction.links->Array.mapPartial(State.Construction.getLink(c))
  let constructs =
    tokens->Array.filter(((id, _)) => links->Array.some(l => l->EdgeData.source === id)->not)
  let tokens = tokens->Gid.Map.fromArray
  let constructors = constructors->Gid.Map.fromArray

  let rec makeConstructions = (construct, usedTokens) => {
    switch usedTokens->Gid.Map.get(construct) {
    // If we've already seen this token, reference it.
    | Some(tok) => Or_error.create([(Reference(tok), usedTokens)])
    | None => {
        let tok =
          tokens
          ->Gid.Map.get(construct)
          ->Or_error.fromOption_s("token data missing")
          ->Or_error.flatMap(td => CSpace.tokenFromTokenData(construct, td))
        let inputCons = links->Array.keepMap(l =>
          if EdgeData.target(l) === construct {
            Some((EdgeData.payload(l), EdgeData.source(l)))
          } else {
            None
          }
        )
        let usedTokens =
          tok
          ->Or_error.map(tok => usedTokens->Gid.Map.set(construct, tok))
          ->Or_error.getWithDefault(usedTokens)
        // If there's no "inputs" to this token, we call it a source
        if inputCons == [] {
          tok->Or_error.map(tok => [(Source(tok), usedTokens)])
        } else {
          // Otherwise, for each possible constructor input...
          inputCons
          ->Array.map(((payload, cid)) => {
            let cons =
              constructors
              ->Gid.Map.get(cid)
              ->Option.flatMap(c => c.constructor)
              ->Or_error.fromOption_s("constructor data missing")
            (tok, cons)
            ->Or_error.both
            ->Or_error.flatMap(tc => {
              if Option.isSome(payload) {
                Or_error.error_s("Arrows from constructors should be unlabelled")
              } else {
                Or_error.create(tc)
              }
            })
            ->Or_error.flatMap(((tok, cons)) => {
              let tc = {
                token: tok,
                constructor: cons,
              }
              // ... we find all input tokens...
              links
              ->Array.keepMap(l =>
                if EdgeData.target(l) === cid {
                  EdgeData.payload(l)
                  ->Or_error.fromOption_s("Arrow has no number")
                  ->Or_error.flatMap(idx =>
                    if idx <= 0 {
                      Or_error.error_ss([
                        "Arrows into constructors must be labelled with positive numbers, not ",
                        Int.toString(idx),
                      ])
                    } else {
                      Or_error.create(idx)
                    }
                  )
                  ->Or_error.map(idx => (idx, EdgeData.source(l)))
                  ->Some
                } else {
                  None
                }
              )
              ->Or_error.allArray
              ->Or_error.flatMap(arrows => {
                // Check that the incoming arrows are all unique
                if (
                  arrows->Array.map(fst)->Belt.Set.Int.fromArray->Belt.Set.Int.size ===
                    Array.length(arrows)
                ) {
                  Or_error.create(arrows)
                } else {
                  Or_error.error_s("Arrows have duplicated values")
                }
              })
              ->Or_error.flatMap(arrows => {
                let count = Array.length(arrows)
                if arrows->Array.map(fst)->Array.some(idx => idx > count) {
                  Or_error.error_s("Arrows into a constructor must be numbered 1 to n")
                } else {
                  Or_error.create(arrows)
                }
              })
              ->Or_error.flatMap(inputToks => {
                inputToks->Js.Array2.sortInPlaceWith(((i, _), (j, _)) => i - j)->ignore
                let inputToks = inputToks->Array.map(snd)
                // ... then recursively find all ways to make those
                let rec buildConstructionsForTokens = (i, usedTokens, result) => {
                  if i === Array.length(inputToks) {
                    [(Or_error.create(result), usedTokens)]
                  } else {
                    let tokId = inputToks[i]->Option.getExn
                    switch makeConstructions(tokId, usedTokens)->Or_error.match {
                    | Or_error.Ok(options) =>
                      options->Array.flatMap(((cons, usedTokens)) =>
                        buildConstructionsForTokens(i + 1, usedTokens, result->Array.push(cons))
                      )
                    | Or_error.Err(e) => [(Or_error.error(e), usedTokens)]
                    }
                  }
                }
                // We end up with all possible ways to build this constructor!
                buildConstructionsForTokens(0, usedTokens, [])
                ->Array.map(((inputConstructions, usedTokens)) =>
                  inputConstructions->Or_error.map(ic => (TCPair(tc, ic), usedTokens))
                )
                ->Or_error.allArray
              })
            })
          })
          // We then flatten all combinations of constructors and input tokens
          ->Or_error.allArray
          ->Or_error.map(Array.concatMany)
        }
      }
    }
  }

  if constructs == [] {
    Or_error.error_s("Structure graph has no constructs!")
  } else {
    constructs
    ->Array.map(((id, _)) => makeConstructions(id, Gid.Map.empty()))
    ->Or_error.allArray
    ->Or_error.map(Array.concatMany)
    ->Or_error.map(Array.map(_, fst))
  }
}

let url = s => "core.construction." ++ s

let toOrugaString = Rpc_service.require(url("toString"), construction_rpc, String.t_rpc)
