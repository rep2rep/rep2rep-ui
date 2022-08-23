type t = {
  constructor: option<CSpace.constructor>,
  notes: string,
}

let create: (~notes: string=?, unit) => t
let duplicate: t => t
let hash: t => Hash.t

module Stable: {
  module V1: {
    type t = t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
