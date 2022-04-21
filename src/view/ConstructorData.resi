type t = {
  constructor: option<CSpace.constructor>,
  notes: string,
}

let create: (~notes: string=?, unit) => t
let duplicate: t => t
