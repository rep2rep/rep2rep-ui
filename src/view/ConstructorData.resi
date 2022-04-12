type t = {
  label: string,
  notes: string,
}

let create: (~notes: string=?, string) => t
let duplicate: t => t
