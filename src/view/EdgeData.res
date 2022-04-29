type t = {
  source: Gid.t,
  target: Gid.t,
  payload: option<int>,
  notes: string,
}

let create = (source, target, payload) => {
  source: source,
  target: target,
  payload: payload,
  notes: "",
}

let duplicate = t => {
  source: t.source,
  target: t.target,
  payload: t.payload,
  notes: t.notes,
}

let source = t => t.source
let target = t => t.target
let payload = t => t.payload
let notes = t => t.notes

let setSource = (t, source) => {...t, source: source}
let setTarget = (t, target) => {...t, target: target}
let setPayload = (t, payload) => {...t, payload: payload}
let setNotes = (t, notes) => {...t, notes: notes}
