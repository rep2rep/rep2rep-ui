type t = {
  label: string,
  payload: option<(string, float, float)>,
  type_: option<Type.typ>,
  subtype: option<string>,
  notes: string,
}

let create: (
  ~payload: (string, float, float)=?,
  ~type_: Type.typ=?,
  ~subtype: string=?,
  ~notes: string=?,
  string,
) => t
let duplicate: t => t
let hash: t => Hash.t

let setPayload: (t, option<(string, float, float)>) => t

let size: t => {"width": float, "height": float}
let render: t => React.element

module Stable: {
  module V1: {
    type t = t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
