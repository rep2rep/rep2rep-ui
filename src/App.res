module App = {

  @react.component
  let make = () => {
    <h1> {React.string("Rep2Rep Online")} </h1>
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
