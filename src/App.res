module App = {
  let forAaron: unit => Rpc.Response.t<Constructions.construction> = Rpc_service.require(
    "aarons.forAaron",
    Rpc.Datatype.unit_,
    Constructions.construction_rpc,
  )

  @react.component
  let make = () => {
    forAaron()->Rpc.Response.upon(construction => {
      Js.Console.log(construction)
      construction->Constructions.size->Rpc.Response.upon(size => Js.Console.log({"size": size}))
      construction
      ->Constructions.leavesOfConstruction
      ->Rpc.Response.upon(leaves => Js.Console.log({"leaves": leaves}))
      construction
      ->Constructions.fullTokenSequence
      ->Rpc.Response.upon(seq => Js.Console.log({"full token sequence": seq}))
    })
    <h1> {React.string("Rep2Rep Online")} </h1>
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
