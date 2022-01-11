type method =
  | GET
  | POST

let methodToString = m =>
  switch m {
  | GET => "GET"
  | POST => "POST"
  }

type params<'body> = {
  method: string,
  body: option<'body>,
}

module Response = {
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

  @send
  external arrayBufferExternal: t<'body> => Promise.t<Js.TypedArray2.ArrayBuffer.t> = "arrayBuffer"

  let arrayBuffer = t =>
    arrayBufferExternal(t)->Promise.thenResolve(Js.TypedArray2.Uint8Array.fromBuffer)
}

@val external fetchExternal: (string, params<'a>) => Promise.t<Response.t<'b>> = "fetch"

let fetch = (url, ~method=GET, ~body=?, ()) => {
  let params = {
    method: methodToString(method),
    body: body,
  }
  fetchExternal(url, params)
}
