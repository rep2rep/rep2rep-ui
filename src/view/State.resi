module Construction: {
  module Metadata: {
    type t

    let name: t => string
    let notes: t => string
    let setName: (t, string) => t
    let setNotes: (t, string) => t
  }

  type t

  let graph: t => GraphState.t
  let metadata: t => Metadata.t
  let tokens: t => array<Gid.t>
  let constructors: t => array<Gid.t>
  let links: t => array<Gid.t>
  let space: t => option<string> // The space _name_

  let isEmpty: t => bool

  let updateMetadata: (t, Metadata.t => Metadata.t) => t
  let setSpace: (t, option<string>) => t
  let addToken: (t, Gid.t, ~x: float, ~y: float) => t
  let addConstructor: (t, Gid.t, ~x: float, ~y: float) => t
  let updateToken: (t, Gid.t, TokenData.t => TokenData.t) => t
  let updateConstructor: (t, Gid.t, ConstructorData.t => ConstructorData.t) => t
  let updateEdge: (t, Gid.t, EdgeData.t => EdgeData.t) => t
  let duplicateNode: (t, ~oldId: Gid.t, ~newId: Gid.t) => t
  let connect: (t, ~linkId: Gid.t, ~source: Gid.t, ~target: Gid.t) => t
  let moveNode: (t, Gid.t, ~x: float, ~y: float) => t
  let deleteNode: (t, Gid.t) => t
  let deleteLink: (t, Gid.t) => t
  let setSelection: (t, GraphState.Selection.t) => t
  let getNode: (t, Gid.t) => option<[#token(TokenData.t) | #constructor(ConstructorData.t)]>
  let getLink: (t, Gid.t) => option<EdgeData.t>

  let toOruga: t => Or_error.t<array<Constructions.construction>>
  let fromOruga: (array<Constructions.construction>, ~space: string) => Or_error.t<t>

  let typeCheck: t => Or_error.t<Rpc.Response.t<Result.t<unit, array<Diagnostic.t>>>>
  let transfer: (
    t,
    ~targetSpace: string,
    ~interSpace: string,
  ) => Rpc.Response.t<Result.t<t, array<Diagnostic.t>>>

  module Stable: {
    module V1: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

type t

let hash: t => Hash.t

let empty: t
let focused: t => option<Gid.t>
let constructions: t => FileTree.t<(Gid.t, Construction.t)>
let construction: (t, Gid.t) => option<Construction.t>
let pathForConstruction: (t, Gid.t) => option<FileTree.Path.t>
let spaces: t => String.Map.t<CSpace.conSpec>
let getSpace: (t, string) => option<CSpace.conSpec>
let typeSystems: t => String.Map.t<FiniteSet.t<Type.PrincipalType.t>>
let renderable: (t, string) => bool
let renderConstruction: (t, Gid.t) => Rpc.Response.t<Result.t<Construction.t, array<Diagnostic.t>>>
let allowedTransfers: (t, string) => String.Map.t<array<string>>

let loadSpaces: t => Rpc.Response.t<t>
let loadTypeSystems: t => Rpc.Response.t<t>
let loadRenderers: t => Rpc.Response.t<t>
let loadAllowedTransfers: t => Rpc.Response.t<t>

let setDB: (IndexedDB.t, string) => unit

let store: t => unit
let load: (~atTime: float) => Promise.t<option<t>>
let isValid: t => Result.t<unit, array<string>>

let newConstruction: (t, Gid.t, string, FileTree.Path.t, ~atTime: float) => t
let deleteConstruction: (t, Gid.t) => t
let focusConstruction: (t, option<Gid.t>) => t
let duplicateConstruction: (t, ~oldId: Gid.t, ~newId: Gid.t, ~atTime: float) => t
let reorderConstructions: (t, FileTree.t<Gid.t>) => t
let importConstruction: (t, Gid.t, Construction.t, FileTree.Path.t, ~atTime: float) => t
let updateConstruction: (t, Gid.t, Construction.t => Construction.t, ~atTime: float) => t
let updateConstructionBypassingUndoRedo: (t, Gid.t, Construction.t => Construction.t) => t

let newFolder: (t, Gid.t, string, FileTree.Path.t) => t
let renameFolder: (t, Gid.t, string) => t
let deleteFolder: (t, Gid.t) => t

let undo: (t, Gid.t) => t
let redo: (t, Gid.t) => t
let canUndo: (t, Gid.t) => bool
let canRedo: (t, Gid.t) => bool
