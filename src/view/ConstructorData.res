type t = {
  label: string,
  notes: string,
}

let create = (~notes="", label) => {label: label, notes: notes}
let duplicate = t => {
  label: t.label,
  notes: t.notes,
}
