module Token = {
  type t =
    | Label(string)
    | Type(option<Type.typ>)
    | Subtype(option<string>)
    | Notes(string)

  let dispatch = (token, t) =>
    switch t {
    | Label(l) => {...token, TokenData.label: l}
    | Type(t) => {...token, TokenData.type_: t}
    | Subtype(s) => {...token, subtype: s}
    | Notes(n) => {...token, TokenData.notes: n}
    }

  let shouldTriggerIntelligence = t =>
    switch t {
    | Type(_)
    | Subtype(_) => true
    | Label(_)
    | Notes(_) => false
    }
}

module Constructor = {
  type t =
    | Constructor(option<CSpace.constructor>)
    | Notes(string)

  let dispatch = (constructor, t) =>
    switch t {
    | Constructor(l) => {...constructor, ConstructorData.constructor: l}
    | Notes(n) => {...constructor, ConstructorData.notes: n}
    }

  let shouldTriggerIntelligence = t =>
    switch t {
    | Constructor(_) => true
    | Notes(_) => false
    }
}

module Edge = {
  type t =
    | Value(option<int>)
    | Notes(string)

  let dispatch = (edge, t) =>
    switch t {
    | Value(v) => edge->EdgeData.setPayload(v)
    | Notes(n) => edge->EdgeData.setNotes(n)
    }

  let shouldTriggerIntelligence = t =>
    switch t {
    | Value(_) => true
    | Notes(_) => false
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

  type rec t =
    | Multiple(array<t>)
    | SetSpace(option<string>)
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
    | UpdateEdge(Gid.t, Edge.t)
    | Replace(State.Construction.t)

  let rec dispatch = (construction, t) =>
    switch t {
    | Multiple(ts) => ts->Array.reduce(construction, dispatch)
    | SetSpace(space) => construction->State.Construction.setSpace(space)
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
    | UpdateEdge(id, ev) =>
      construction->State.Construction.updateEdge(id, ed => ed->Edge.dispatch(ev))
    | Replace(c2) => c2
    }

  let rec shouldTriggerIntelligence = t =>
    switch t {
    | Multiple(ts) => ts->Array.some(shouldTriggerIntelligence)
    | SetSpace(_)
    | AddToken(_, _, _)
    | AddConstructor(_, _, _)
    | DuplicateNode(_, _)
    | ConnectNodes(_, _, _)
    | DeleteNode(_)
    | DeleteLink(_)
    | Replace(_) => true
    | MoveNode(_, _, _)
    | ChangeSelection(_)
    | UpdateMetadata(_) => false
    | UpdateToken(_, e) => Token.shouldTriggerIntelligence(e)
    | UpdateConstructor(_, e) => Constructor.shouldTriggerIntelligence(e)
    | UpdateEdge(_, e) => Edge.shouldTriggerIntelligence(e)
    }
}

type t =
  | Update(State.t)
  | NewConstruction(Gid.t, string, FileTree.Path.t)
  | DeleteConstruction(Gid.t)
  | FocusConstruction(option<Gid.t>)
  | DuplicateConstruction(Gid.t, Gid.t)
  | ReorderConstructions(FileTree.t<Gid.t>)
  | ImportConstruction(Gid.t, State.Construction.t, FileTree.Path.t)
  | NewFolder(Gid.t, string, FileTree.Path.t)
  | RenameFolder(Gid.t, string)
  | DeleteFolder(Gid.t)
  | Undo(Gid.t)
  | Redo(Gid.t)
  | ConstructionEvent(Gid.t, Construction.t)

let dispatch = (state, t, ~atTime) =>
  switch t {
  | Update(s) => s
  | NewConstruction(id, name, path) => state->State.newConstruction(id, name, path, ~atTime)
  | DeleteConstruction(id) => state->State.deleteConstruction(id)
  | FocusConstruction(id) => state->State.focusConstruction(id)
  | DuplicateConstruction(oldId, newId) =>
    state->State.duplicateConstruction(~oldId, ~newId, ~atTime)
  | ReorderConstructions(order) => state->State.reorderConstructions(order)
  | ImportConstruction(id, construction, path) =>
    state->State.importConstruction(id, construction, path, ~atTime)
  | NewFolder(id, name, path) => state->State.newFolder(id, name, path)
  | RenameFolder(id, name) => state->State.renameFolder(id, name)
  | DeleteFolder(id) => state->State.deleteFolder(id)
  | Undo(id) => state->State.undo(id)
  | Redo(id) => state->State.redo(id)
  | ConstructionEvent(id, event) =>
    state->State.updateConstruction(id, Construction.dispatch(_, event), ~atTime)
  }

let shouldTriggerIntelligence = t =>
  switch t {
  | Update(_)
  | NewConstruction(_, _, _)
  | DeleteConstruction(_)
  | FocusConstruction(_)
  | DuplicateConstruction(_, _)
  | ImportConstruction(_, _, _)
  | Undo(_)
  | Redo(_) => true
  | ReorderConstructions(_)
  | NewFolder(_, _, _)
  | RenameFolder(_, _)
  | DeleteFolder(_) => false
  | ConstructionEvent(_, e) => Construction.shouldTriggerIntelligence(e)
  }
