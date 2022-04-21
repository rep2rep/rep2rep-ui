type t = {
  constructor: option<CSpace.constructor>,
  notes: string,
}

let create = (~notes="", ()) => {constructor: None, notes: notes}
let duplicate = t => {
  constructor: t.constructor,
  notes: t.notes,
}
