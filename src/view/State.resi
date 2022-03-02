module Construction: {
  module Metadata: {
    type t

    let name: t => string
  }

  type t

  let graph: t => GraphState.t
  let metadata: t => Metadata.t

  let rename: (t, string) => t
  let addToken: (t, Uuid.t, ~x: float, ~y: float) => t
  let addConstructor: (t, Uuid.t, ~x: float, ~y: float) => t
  let duplicateNode: (t, ~oldId: Uuid.t, ~newId: Uuid.t) => t
  let connect: (t, ~linkId: Uuid.t, ~source: Uuid.t, ~target: Uuid.t) => t
  let moveNode: (t, Uuid.t, ~x: float, ~y: float) => t
  let deleteNode: (t, Uuid.t) => t
  let deleteLink: (t, Uuid.t) => t
  let setSelection: (t, GraphState.Selection.t) => t
}

type t

let empty: t
let focused: t => option<Uuid.t>
let constructions: t => array<(Uuid.t, Construction.t)>
let construction: (t, Uuid.t) => option<Construction.t>

let load: unit => option<t>
let store: t => unit

let newConstruction: (t, Uuid.t, string) => t
let deleteConstruction: (t, Uuid.t) => t
let focusConstruction: (t, option<Uuid.t>) => t
let duplicateConstruction: (t, ~oldId: Uuid.t, ~newId: Uuid.t) => t
let reorderConstructions: (t, array<Uuid.t>) => t
let importConstruction: (t, Uuid.t, Construction.t) => t
let updateConstruction: (t, Uuid.t, Construction.t => Construction.t) => t

let undo: (t, Uuid.t) => t
let redo: (t, Uuid.t) => t
let canUndo: (t, Uuid.t) => bool
let canRedo: (t, Uuid.t) => bool
