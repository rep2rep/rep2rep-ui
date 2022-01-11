type method =
  | GET
  | POST
type params<'body>

let methodToString: method => string

module Response: {
  type t<'body> = {
    ok: bool,
    status: int,
    statusText: string,
    @as("type") type_: string,
    url: string,
    redirected: bool,
    headers: Js.Dict.t<string>,
    body: 'body,
    bodyUsed: bool,
  }

  let arrayBuffer: t<'body> => Promise.t<Js.TypedArray2.Uint8Array.t>
}

let fetch: (string, ~method: method=?, ~body: 'a=?, unit) => Promise.t<Response.t<'b>>
