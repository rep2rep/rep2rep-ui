type t = {
  label: string,
  payload: option<string>,
  type_: option<Type.typ>,
  subtype: option<string>,
  notes: string,
}

let create: (
  ~payload: string=?,
  ~type_: Type.typ=?,
  ~subtype: string=?,
  ~notes: string=?,
  string,
) => t
let duplicate: t => t

let size: t => {"width": float, "height": float}
let render: t => React.element
