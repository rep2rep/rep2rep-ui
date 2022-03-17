module Construction: {
  module Metadata: {
    type t

    let name: t => string
  }

  type t

  let graph: t => GraphState.t
  let metadata: t => Metadata.t

  let rename: (t, string) => t
  let addToken: (t, Gid.t, ~x: float, ~y: float) => t
  let addConstructor: (t, Gid.t, ~x: float, ~y: float) => t
  let duplicateNode: (t, ~oldId: Gid.t, ~newId: Gid.t) => t
  let connect: (t, ~linkId: Gid.t, ~source: Gid.t, ~target: Gid.t) => t
  let moveNode: (t, Gid.t, ~x: float, ~y: float) => t
  let deleteNode: (t, Gid.t) => t
  let deleteLink: (t, Gid.t) => t
  let setSelection: (t, GraphState.Selection.t) => t
}

type t

let empty: t
let focused: t => option<Gid.t>
let constructions: t => array<(Gid.t, Construction.t)>
let construction: (t, Gid.t) => option<Construction.t>

let load: unit => option<t>
let store: t => unit

let newConstruction: (t, Gid.t, string) => t
let deleteConstruction: (t, Gid.t) => t
let focusConstruction: (t, option<Gid.t>) => t
let duplicateConstruction: (t, ~oldId: Gid.t, ~newId: Gid.t) => t
let reorderConstructions: (t, array<Gid.t>) => t
let importConstruction: (t, Gid.t, Construction.t) => t
let updateConstruction: (t, Gid.t, Construction.t => Construction.t) => t

let undo: (t, Gid.t) => t
let redo: (t, Gid.t) => t
let canUndo: (t, Gid.t) => bool
let canRedo: (t, Gid.t) => bool
