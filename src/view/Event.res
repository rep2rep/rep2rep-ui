module Construction = {
  type t =
    | Rename(string)
    | AddToken(Uuid.t, float, float)
    | AddConstructor(Uuid.t, float, float)
    | DuplicateNode(Uuid.t, Uuid.t)
    | ConnectNodes(Uuid.t, Uuid.t, Uuid.t)
    | MoveNode(Uuid.t, float, float)
    | DeleteNode(Uuid.t)
    | DeleteLink(Uuid.t)
    | ChangeSelection(GraphState.Selection.t)

  let dispatch = (construction, t) =>
    switch t {
    | Rename(name) => construction->State.Construction.rename(name)
    | AddToken(id, x, y) => construction->State.Construction.addToken(id, ~x, ~y)
    | AddConstructor(id, x, y) => construction->State.Construction.addConstructor(id, ~x, ~y)
    | DuplicateNode(oldId, newId) => construction->State.Construction.duplicateNode(~oldId, ~newId)
    | ConnectNodes(linkId, source, target) =>
      construction->State.Construction.connect(~linkId, ~source, ~target)
    | MoveNode(id, x, y) => construction->State.Construction.moveNode(id, ~x, ~y)
    | DeleteNode(id) => construction->State.Construction.deleteNode(id)
    | DeleteLink(linkId) => construction->State.Construction.deleteLink(linkId)
    | ChangeSelection(selection) => construction->State.Construction.setSelection(selection)
    }
}

type t =
  | NewConstruction(Uuid.t, string)
  | DeleteConstruction(Uuid.t)
  | FocusConstruction(option<Uuid.t>)
  | DuplicateConstruction(Uuid.t, Uuid.t)
  | ReorderConstructions(array<Uuid.t>)
  | ImportConstruction(Uuid.t, State.Construction.t)
  | Undo(Uuid.t)
  | Redo(Uuid.t)
  | ConstructionEvent(Uuid.t, Construction.t)

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
