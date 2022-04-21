module Token = {
  type t =
    | Label(string)
    | Notes(string)

  let dispatch = (token, t) =>
    switch t {
    | Label(l) => {...token, TokenData.label: l}
    | Notes(n) => {...token, TokenData.notes: n}
    }
}

module Constructor = {
  type t =
    | Label(string)
    | Notes(string)

  let dispatch = (constructor, t) =>
    switch t {
    | Label(l) => {...constructor, ConstructorData.label: l}
    | Notes(n) => {...constructor, ConstructorData.notes: n}
    }
}

module Construction = {
  module Metadata = {
    type t =
      | Name(string)
      | Notes(string)

    let dispatch = (metadata, t) =>
      switch t {
      | Name(n) => metadata->State.Construction.Metadata.setName(n)
      | Notes(n) => metadata->State.Construction.Metadata.setNotes(n)
      }
  }

  type t =
    | AddToken(Gid.t, float, float)
    | AddConstructor(Gid.t, float, float)
    | DuplicateNode(Gid.t, Gid.t)
    | ConnectNodes(Gid.t, Gid.t, Gid.t)
    | MoveNode(Gid.t, float, float)
    | DeleteNode(Gid.t)
    | DeleteLink(Gid.t)
    | ChangeSelection(GraphState.Selection.t)
    | UpdateMetadata(Metadata.t)
    | UpdateToken(Gid.t, Token.t)
    | UpdateConstructor(Gid.t, Constructor.t)

  let dispatch = (construction, t) =>
    switch t {
    | AddToken(id, x, y) => construction->State.Construction.addToken(id, ~x, ~y)
    | AddConstructor(id, x, y) => construction->State.Construction.addConstructor(id, ~x, ~y)
    | DuplicateNode(oldId, newId) => construction->State.Construction.duplicateNode(~oldId, ~newId)
    | ConnectNodes(linkId, source, target) =>
      construction->State.Construction.connect(~linkId, ~source, ~target)
    | MoveNode(id, x, y) => construction->State.Construction.moveNode(id, ~x, ~y)
    | DeleteNode(id) => construction->State.Construction.deleteNode(id)
    | DeleteLink(linkId) => construction->State.Construction.deleteLink(linkId)
    | ChangeSelection(selection) => construction->State.Construction.setSelection(selection)
    | UpdateMetadata(ev) =>
      construction->State.Construction.updateMetadata(Metadata.dispatch(_, ev))
    | UpdateToken(id, ev) => construction->State.Construction.updateToken(id, Token.dispatch(_, ev))
    | UpdateConstructor(id, ev) =>
      construction->State.Construction.updateConstructor(id, con => con->Constructor.dispatch(ev))
    }
}

type t =
  | NewConstruction(Gid.t, string)
  | DeleteConstruction(Gid.t)
  | FocusConstruction(option<Gid.t>)
  | DuplicateConstruction(Gid.t, Gid.t)
  | ReorderConstructions(array<Gid.t>)
  | ImportConstruction(Gid.t, State.Construction.t)
  | Undo(Gid.t)
  | Redo(Gid.t)
  | ConstructionEvent(Gid.t, Construction.t)

let dispatch = (state, t) =>
  switch t {
  | NewConstruction(id, name) => state->State.newConstruction(id, name)
  | DeleteConstruction(id) => state->State.deleteConstruction(id)
  | FocusConstruction(id) => state->State.focusConstruction(id)
  | DuplicateConstruction(oldId, newId) => state->State.duplicateConstruction(~oldId, ~newId)
  | ReorderConstructions(order) => state->State.reorderConstructions(order)
  | ImportConstruction(id, construction) => state->State.importConstruction(id, construction)
  | Undo(id) => state->State.undo(id)
  | Redo(id) => state->State.redo(id)
  | ConstructionEvent(id, event) =>
    state->State.updateConstruction(id, Construction.dispatch(_, event))
  }
