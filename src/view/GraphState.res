let toGid = nodeId => nodeId->ReactD3Graph.Node.Id.toString->Gid.fromString
let fromGid = uuid => uuid->Gid.toString->ReactD3Graph.Node.Id.ofString

module GraphNode = {
  type nodeData =
    | Token(TokenData.t)
    | Constructor(ConstructorData.t)

  let hash_nodeData = nd =>
    switch nd {
    | Token(data) => [String.hash("token"), TokenData.hash(data)]->Hash.combine
    | Constructor(data) => [String.hash("constructor"), ConstructorData.hash(data)]->Hash.combine
    }

  type t = (ReactD3Graph.Node.t<[#token | #constructor]>, nodeData)

  let isValid = ((_, d)) =>
    switch d {
    | Token(d) => TokenData.isValid(d)
    | Constructor(d) => ConstructorData.isValid(d)
    }

  let hash = ((node, data)) => {
    let id = ReactD3Graph.Node.id(node)->toGid
    let x = ReactD3Graph.Node.x(node)
    let y = ReactD3Graph.Node.y(node)
    let payload = ReactD3Graph.Node.payload(node)
    let hash_payload = p =>
      switch p {
      | #token => String.hash("token")
      | #constructor => String.hash("constructor")
      }
    Hash.combine([
      Gid.hash(id),
      Float.hash(x),
      Float.hash(y),
      Option.hash(payload, hash_payload),
      hash_nodeData(data),
    ])
  }

  let unselectedFill = "white"
  let selectedFill = "rgb(220, 220, 220)"
  let unselectedStrokeWidth = "1"
  let selectedStrokeWidth = "2"

  let tokenConfig = data => {
    let size = TokenData.size(data)
    ReactD3Graph.Node.Config.create(
      ~size={"width": size["width"] *. 10. +. 20., "height": size["height"] *. 10. +. 20.},
      ~viewGenerator=node => {
        <>
          <rect
            x={"1"}
            y={"1"}
            width={size["width"]->Float.toString}
            height={size["height"]->Float.toString}
            rx="7"
            ry="7"
            fill={if ReactD3Graph.Node.selected(node) {
              selectedFill
            } else {
              unselectedFill
            }}
            stroke="black"
            strokeWidth={if ReactD3Graph.Node.selected(node) {
              selectedStrokeWidth
            } else {
              unselectedStrokeWidth
            }}
          />
          <g x={"1"} y={"1"}> {TokenData.render(data)} </g>
        </>
      },
      (),
    )
  }
  let constructorConfig = (content: ConstructorData.t) =>
    ReactD3Graph.Node.Config.create(
      ~size={
        "height": 190.,
        "width": 170. +.
        String.approximateEmWidth(
          content.constructor->Option.map(CSpace.constructorName)->Option.getWithDefault("-"),
        ) *. 150.,
      },
      ~viewGenerator=node => {
        <>
          <circle
            cx="6"
            cy="13"
            r="5"
            fill={if ReactD3Graph.Node.selected(node) {
              selectedFill
            } else {
              unselectedFill
            }}
            stroke="black"
            strokeWidth={if ReactD3Graph.Node.selected(node) {
              selectedStrokeWidth
            } else {
              unselectedStrokeWidth
            }}
          />
          <text x="15" y="12">
            {React.string(
              content.constructor->Option.map(CSpace.constructorName)->Option.getWithDefault("-"),
            )}
          </text>
        </>
      },
      (),
    )

  let create = (id, ~x, ~y, nodeData) => {
    let (config, payload) = switch nodeData {
    | Token(data) => (tokenConfig(data), #token)
    | Constructor(data) => (constructorConfig(data), #constructor)
    }
    (ReactD3Graph.Node.create(~id=fromGid(id), ~payload, ~x, ~y, ~config, ()), nodeData)
  }

  let data = ((t, _)) => [t]
  let id = ((t, _)) => ReactD3Graph.Node.id(t)->toGid
  let duplicate = ((t, p), newId) => (t->ReactD3Graph.Node.setId(newId->fromGid), p)
  let move = ((t, p), ~x, ~y) => (t->ReactD3Graph.Node.setX(x)->ReactD3Graph.Node.setY(y), p)
  let setData = ((t, _), newPayload) => {
    let config = switch newPayload {
    | Token(data) => tokenConfig(data)
    | Constructor(data) => constructorConfig(data)
    }
    (ReactD3Graph.Node.updateConfig(t, _ => config), newPayload)
  }
  let kind = ((_, d)) =>
    switch d {
    | Token(_) => #token
    | Constructor(_) => #constructor
    }

  module Stable = {
    module V1 = {
      type nodeData = nodeData =
        | Token(TokenData.Stable.V1.t)
        | Constructor(ConstructorData.Stable.V1.t)
      type t = (ReactD3Graph.Node.t<[#token | #constructor]>, nodeData)

      let toJson = ((t, p)) =>
        Js.Dict.fromArray([
          ("version", 1->Int.toJson),
          ("id", t->ReactD3Graph.Node.id->toGid->Gid.toJson),
          (
            "kind",
            switch p {
            | Token(_) => "token"->String.toJson
            | Constructor(_) => "constructor"->String.toJson
            },
          ),
          (
            "payload",
            switch p {
            | Token(d) => d->TokenData.Stable.V1.toJson
            | Constructor(d) => d->ConstructorData.Stable.V1.toJson
            },
          ),
          ("x", t->ReactD3Graph.Node.x->Float.toJson),
          ("y", t->ReactD3Graph.Node.y->Float.toJson),
        ])->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode GraphNode object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let id = getValue("id", Gid.fromJson)
              let kind = getValue("kind", j =>
                j
                ->String.fromJson
                ->Or_error.flatMap(s =>
                  switch s {
                  | "token" => #token->Or_error.create
                  | "constructor" => #constructor->Or_error.create
                  | _ => Or_error.error_ss(["Unknown GraphNode kind: ", s])
                  }
                )
              )
              let payload =
                (kind, getValue("payload", Or_error.create))
                ->Or_error.both
                ->Or_error.flatMap(((kind, payload)) =>
                  switch kind {
                  | #token => payload->TokenData.Stable.V1.fromJson->Or_error.map(t => Token(t))
                  | #constructor =>
                    payload->ConstructorData.Stable.V1.fromJson->Or_error.map(c => Constructor(c))
                  }
                )
              let x = getValue("x", Float.fromJson)
              let y = getValue("y", Float.fromJson)
              (id, payload, x, y)
              ->Or_error.both4
              ->Or_error.map(((id, payload, x, y)) => create(id, ~x, ~y, payload))
            }
          | Or_error.Ok(v) => Or_error.error_ss(["Unknown GraphNode version ", Int.toString(v)])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }
}

module GraphLink = {
  type t = (ReactD3Graph.Link.t<unit>, EdgeData.t)

  let isValid = ((l, d)) => {
    let dataValid = EdgeData.isValid(d)
    let id =
      ReactD3Graph.Link.id(l)->Option.map(ReactD3Graph.Link.Id.toString)->Option.map(Gid.fromString)
    let idValid = if id->Option.isSome {
      Result.Ok()
    } else {
      Result.Error(["Link has no ID!"])
    }
    let sourceValid = if l->ReactD3Graph.Link.source->toGid == EdgeData.source(d) {
      Result.Ok()
    } else {
      Result.Error([
        "Link (" ++
        id->Option.map(Gid.toString)->Option.getWithDefault("<NO_LINK_ID>") ++
        ") sources do not match between graph and data: " ++
        l->ReactD3Graph.Link.source->toGid->Gid.toString ++
        " vs " ++
        EdgeData.source(d)->Gid.toString,
      ])
    }
    let targetValid = if l->ReactD3Graph.Link.target->toGid == EdgeData.target(d) {
      Result.Ok()
    } else {
      Result.Error([
        "Link (" ++
        id->Option.map(Gid.toString)->Option.getWithDefault("<NO_LINK_ID>") ++
        ") targets do not match between graph and data: " ++
        l->ReactD3Graph.Link.target->toGid->Gid.toString ++
        " vs " ++
        EdgeData.target(d)->Gid.toString,
      ])
    }
    [dataValid, idValid, sourceValid, targetValid]->Result.allUnit(Array.concatMany)
  }

  let hash = ((link, data)) => {
    let source = ReactD3Graph.Link.source(link)->toGid
    let target = ReactD3Graph.Link.target(link)->toGid
    let id =
      ReactD3Graph.Link.id(link)
      ->Option.map(ReactD3Graph.Link.Id.toString)
      ->Option.map(Gid.fromString)
    Hash.combine([
      Gid.hash(source),
      Gid.hash(target),
      Option.hash(id, Gid.hash),
      EdgeData.hash(data),
    ])
  }

  let makeConfig = edgeData => {
    let offsets = (src, tgt, _) =>
      (src, tgt)
      ->Option.both
      ->Option.map(((src, tgt)) => {
        let p_src = src->ReactD3Graph.Core.readKeyExn("payload")
        let p_tgt = tgt->ReactD3Graph.Core.readKeyExn("payload")
        let ssize = src->ReactD3Graph.Core.readKeyExn("size")
        let tsize = tgt->ReactD3Graph.Core.readKeyExn("size")
        let (x1, y1) = switch p_src {
        | #token => (src["x"], src["y"])
        | #constructor => (
            src["x"] -. ssize["width"] /. 20. +. 6.,
            src["y"] -. ssize["height"] /. 20. +. 13.,
          )
        }
        let (x2, y2) = switch p_tgt {
        | #token => (tgt["x"], tgt["y"])
        | #constructor => (
            tgt["x"] -. tsize["width"] /. 20. +. 6.,
            tgt["y"] -. tsize["height"] /. 20. +. 13.,
          )
        }
        let cons = (~isSource) => {
          let (x, y) = if isSource {
            (x2 -. x1, y2 -. y1)
          } else {
            (x1 -. x2, y1 -. y2)
          }
          let size = if isSource {
            ssize
          } else {
            tsize
          }
          let l = Js.Math.hypot(x, y)
          let (dx, dy) = (x /. l, y /. l)
          {
            "dx": 6. *. dx -. size["width"] /. 20. +. 6.,
            "dy": 6. *. dy -. size["height"] /. 20. +. 13.,
          }
        }
        let tok = (~isSource) => {
          let size = if isSource {
            ssize
          } else {
            tsize
          }
          let (dx0, dy0) = if isSource {
            (x2 -. x1, y2 -. y1)
          } else {
            (x1 -. x2, y1 -. y2)
          }
          let (dx, dy) = if Js.Math.abs_float(dx0 /. dy0) > size["width"] /. size["height"] {
            let dx = size["width"] /. 20. *. Js.Math.sign_float(dx0)
            let dy = dy0 *. dx /. dx0
            (dx, dy)
          } else {
            let dy = size["height"] /. 20. *. Js.Math.sign_float(dy0)
            let dx = dx0 *. dy /. dy0
            (dx, dy)
          }
          {"dx": dx, "dy": dy}
        }
        switch p_src {
        | #constructor => (cons(~isSource=true), tok(~isSource=false))
        | #token => (tok(~isSource=true), cons(~isSource=false))
        }
      })
      ->Option.getWithDefault(({"dx": 0.0, "dy": 0.0}, {"dx": 0.0, "dy": 0.0}))

    ReactD3Graph.Link.Config.create(
      ~offsetSource={
        (src, tgt, breakPoints) => offsets(src, tgt, breakPoints)->fst
      },
      ~offsetTarget={
        (src, tgt, breakPoints) => offsets(src, tgt, breakPoints)->snd
      },
      ~labelProperty=_ =>
        edgeData
        ->EdgeData.payload
        ->Option.map(Int.toString)
        ->Option.map(label => {
          <g>
            <circle r="8" cx="8" cy="8" fill="white" />
            <text textAnchor="middle" x="8" y="12" fontSize="0.8rem"> {React.string(label)} </text>
          </g>
        })
        ->Option.getWithDefault(React.null),
      (),
    )
  }

  let create = (id, ~source, ~target, ~edgeData) => (
    ReactD3Graph.Link.create(
      ~source=fromGid(source),
      ~target=fromGid(target),
      ~id=Gid.toString(id)->ReactD3Graph.Link.Id.ofString,
      ~payload=(),
      ~config=makeConfig(edgeData),
      (),
    ),
    edgeData,
  )

  let data = ((t, _)) => [t]
  let id = ((t, _)) =>
    ReactD3Graph.Link.id(t)->Option.getExn->ReactD3Graph.Link.Id.toString->Gid.fromString
  let source = ((t, _)) => ReactD3Graph.Link.source(t)->toGid
  let target = ((t, _)) => ReactD3Graph.Link.target(t)->toGid
  let duplicate = ((t, p), newId, newSource, newTarget) => (
    t
    ->ReactD3Graph.Link.setId(newId->Gid.toString->ReactD3Graph.Link.Id.ofString)
    ->ReactD3Graph.Link.setSource(newSource->fromGid)
    ->ReactD3Graph.Link.setTarget(newTarget->fromGid),
    p->EdgeData.setSource(newSource)->EdgeData.setTarget(newTarget),
  )
  let setData = ((t, _), newPayload) => {
    let config = makeConfig(newPayload)
    (ReactD3Graph.Link.updateConfig(t, _ => config), newPayload)
  }

  module Stable = {
    module V1 = {
      type t = (ReactD3Graph.Link.t<unit>, EdgeData.Stable.V1.t)

      let toJson = ((t, p)) =>
        Js.Dict.fromArray([
          ("version", 1->Int.toJson),
          (
            "id",
            t
            ->ReactD3Graph.Link.id
            ->Option.getExn
            ->ReactD3Graph.Link.Id.toString
            ->Gid.fromString
            ->Gid.toJson,
          ),
          ("source", t->ReactD3Graph.Link.source->toGid->Gid.toJson),
          ("target", t->ReactD3Graph.Link.target->toGid->Gid.toJson),
          ("payload", p->EdgeData.Stable.V1.toJson),
        ])->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode GraphLink object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let id = getValue("id", Gid.fromJson)
              let source = getValue("source", Gid.fromJson)
              let target = getValue("target", Gid.fromJson)
              let payload = getValue("payload", EdgeData.Stable.V1.fromJson)
              (id, source, target, payload)
              ->Or_error.both4
              ->Or_error.map(((id, source, target, payload)) =>
                create(id, ~source, ~target, ~edgeData=payload)
              )
            }
          | Or_error.Ok(v) => Or_error.error_ss(["Unknown GraphLink version ", Int.toString(v)])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }
}

module Selection = {
  type t = {
    nodes: array<Gid.t>,
    links: array<Gid.t>,
  }

  let hash: t => Hash.t = Hash.record2(
    ("nodes", Array.hash(_, Gid.hash)),
    ("links", Array.hash(_, Gid.hash)),
  )

  let empty = {
    nodes: [],
    links: [],
  }

  let nodes = t => t.nodes
  let links = t => t.links
  let ofNodes = nodes => {nodes: nodes, links: []}
  let toReactD3Selection = t => {
    ReactD3Graph.Graph.Selection.nodes: t.nodes->Array.map(fromGid),
    links: t.links->Array.map(id => id->Gid.toString->ReactD3Graph.Link.Id.ofString),
  }
  let fromReactD3Selection = selection => {
    nodes: selection.ReactD3Graph.Graph.Selection.nodes->Array.map(toGid),
    links: selection.ReactD3Graph.Graph.Selection.links->Array.map(id =>
      id->ReactD3Graph.Link.Id.toString->Gid.fromString
    ),
  }
}

type t = {
  nodes: array<GraphNode.t>,
  links: array<GraphLink.t>,
  selection: Selection.t,
}

let hash = Hash.record3(
  ("nodes", Array.hash(_, GraphNode.hash)),
  ("links", Array.hash(_, GraphLink.hash)),
  ("selection", Selection.hash),
)

let empty = {
  nodes: [],
  links: [],
  selection: Selection.empty,
}

let nodes = t => t.nodes
let links = t => t.links

let isValid = t => {
  let nodesValid = t.nodes->Array.map(GraphNode.isValid)->Result.allUnit(Array.concatMany)
  let linksValid = t.links->Array.map(GraphLink.isValid)->Result.allUnit(Array.concatMany)
  let selectionValid = {
    let nodesValid =
      t.selection
      ->Selection.nodes
      ->Array.map(id =>
        if t.nodes->Array.some(node => GraphNode.id(node) == id) {
          Result.Ok()
        } else {
          Result.Error(["Selection references unknown node: " ++ Gid.toString(id)])
        }
      )
      ->Result.allUnit(Array.concatMany)
    let linksValid =
      t.selection
      ->Selection.links
      ->Array.map(id =>
        if t.links->Array.some(link => GraphLink.id(link) == id) {
          Result.Ok()
        } else {
          Result.Error(["Selection references unknown link: " ++ Gid.toString(id)])
        }
      )
      ->Result.allUnit(Array.concatMany)
    [nodesValid, linksValid]->Result.allUnit(Array.concatMany)
  }
  [nodesValid, linksValid, selectionValid]->Result.allUnit(Array.concatMany)
}

let duplicate = (t, idMap) => {
  nodes: t.nodes->Array.map(node => {
    let id = idMap->Gid.Map.get(GraphNode.id(node))->Option.getExn
    node->GraphNode.duplicate(id)
  }),
  links: t.links->Array.map(link => {
    let id = idMap->Gid.Map.get(GraphLink.id(link))->Option.getExn
    let source = idMap->Gid.Map.get(GraphLink.source(link))->Option.getExn
    let target = idMap->Gid.Map.get(GraphLink.target(link))->Option.getExn
    link->GraphLink.duplicate(id, source, target)
  }),
  selection: Selection.empty,
}

let data = t => {
  ReactD3Graph.Data.nodes: t.nodes->Array.flatMap(GraphNode.data),
  links: t.links->Array.flatMap(GraphLink.data),
}

let selection = t => t.selection
let addNode = (t, node) => {...t, nodes: t.nodes->Array.concat([node])}
let addLink = (t, link) => {...t, links: t.links->Array.concat([link])}

let updateNode = (t, nodeId, f) => {
  ...t,
  nodes: t.nodes->Array.map(node =>
    if GraphNode.id(node) === nodeId {
      f(node)
    } else {
      node
    }
  ),
}

let updateLink = (t, linkId, f) => {
  ...t,
  links: t.links->Array.map(link =>
    if GraphLink.id(link) === linkId {
      f(link)
    } else {
      link
    }
  ),
}

let deleteNode = (t, nodeId) => {
  ...t,
  nodes: t.nodes->Array.filter(node => GraphNode.id(node) !== nodeId),
}

let deleteLink = (t, linkId) => {
  ...t,
  links: t.links->Array.filter(link => GraphLink.id(link) !== linkId),
}

let setSelection = (t, selection) => {...t, selection: selection}

let incidentLinks = (t, ~nodeId) => {
  let inLinks = []
  let outLinks = []
  t.links->Array.forEach(link => {
    if GraphLink.source(link) === nodeId {
      outLinks->Js.Array2.push(GraphLink.id(link))->ignore
    }
    if GraphLink.target(link) === nodeId {
      inLinks->Js.Array2.push(GraphLink.id(link))->ignore
    }
  })
  {"in": inLinks, "out": outLinks}
}

let getNode = (t, nodeId) => t.nodes->Array.find(node => GraphNode.id(node) == nodeId)
let getLink = (t, linkId) => t.links->Array.find(link => GraphLink.id(link) == linkId)

let layout = (~tokens, ~constructors, ~edges) => {
  let toposort = arrows => {
    let sources = arrows->Array.reduce(Gid.Set.empty, (set, (a, _)) => set->Gid.Set.add(a))
    let targets = arrows->Array.reduce(Gid.Set.empty, (set, (_, b)) => set->Gid.Set.add(b))
    // Any subtype that is also a subtype has incoming arrows, so it's not a leaf.
    let leaves = Gid.Set.diff(sources, targets)->Gid.Set.toArray
    let arrows = ref(arrows)
    let result = []
    while leaves != [] {
      let focus = leaves->Js.Array2.shift->Option.getExn
      result->Js.Array2.push(focus)->ignore
      let (outgoing, other) = arrows.contents->Array.partition(((src, _)) => src === focus)
      arrows := other
      outgoing->Array.forEach(((_, target)) =>
        if other->Array.every(((_, tgt)) => target !== tgt) {
          leaves->Js.Array2.push(target)->ignore
        }
      )
    }
    Js.Array2.reverseInPlace(result)->ignore
    result
  }

  let layerize = (toposort, arrows) => {
    let layers = ref(Gid.Map.empty())
    // All are initially on layer 0
    toposort->Array.forEach(id => {layers := layers.contents->Gid.Map.set(id, 0)})
    toposort->Array.forEach(id => {
      // For the i-th element in the toposorted ids, we find all incoming arrows
      let sources = arrows->Array.keepMap(((a, b)) =>
        if b == id {
          Some(a)
        } else {
          None
        }
      )
      // Assume the ith id has layer l, and the layer of the target id is m.
      let current_layer = layers.contents->Gid.Map.get(id)->Option.getExn
      sources->Array.forEach(src => {
        layers :=
          layers.contents->Gid.Map.update(
            src,
            Option.map(
              _,
              // If the m <= l, m := l + 1
              layer =>
                if layer <= current_layer {
                  current_layer + 1
                } else {
                  layer
                },
            ),
          )
      })
    })
    // Group the ids into their layers.
    let n_layers = 1 + layers.contents->Gid.Map.values->Array.reduce(0, Int.max)
    let result = Array.make(n_layers, None)->Array.map(_ => [])
    layers.contents->Gid.Map.forEach((id, l) =>
      result[l]->Option.getExn->Js.Array2.push(id)->ignore
    )
    result
  }

  let tokNodes =
    tokens
    ->Gid.Map.toArray
    ->Array.map(((id, data)) => GraphNode.create(id, ~x=0., ~y=0., GraphNode.Token(data)))
  let consNodes =
    constructors
    ->Gid.Map.toArray
    ->Array.map(((id, data)) => GraphNode.create(id, ~x=0., ~y=0., GraphNode.Constructor(data)))
  let nodes = Array.concat(tokNodes, consNodes)
  let links =
    edges
    ->Gid.Map.toArray
    ->Array.map(((id, data)) =>
      GraphLink.create(
        id,
        ~source=data.EdgeData.source,
        ~target=data.EdgeData.target,
        ~edgeData=data,
      )
    )
  let arrows = links->Array.map(ed => (GraphLink.source(ed), GraphLink.target(ed)))
  let layers = arrows->toposort->layerize(arrows)

  let nodes = layers->Array.reduceWithIndex(nodes, (nodes, ids, layer) => {
    ids->Array.reduceWithIndex(nodes, (nodes, id, dx) =>
      nodes->Array.map(nd =>
        if GraphNode.id(nd) == id {
          nd->GraphNode.move(
            // This is a really crappy layout algorithm for the x-coordinate but it'll do for now
            ~x=100.0 *. Int.toFloat(dx) +. Int.toFloat(mod(layer, 2) * 60),
            ~y=100. *. Int.toFloat(layer),
          )
        } else {
          nd
        }
      )
    )
  })

  {
    nodes: nodes,
    links: links,
    selection: Selection.empty,
  }
}

module Stable = {
  module V1 = {
    type t = t = {
      nodes: array<GraphNode.Stable.V1.t>,
      links: array<GraphLink.Stable.V1.t>,
      selection: Selection.t,
    }

    let toJson = t =>
      Js.Dict.fromArray([
        ("version", 1->Int.toJson),
        ("nodes", t.nodes->Array.toJson(GraphNode.Stable.V1.toJson)),
        ("links", t.links->Array.toJson(GraphLink.Stable.V1.toJson)),
      ])->Js.Json.object_

    let fromJson = json =>
      json
      ->Js.Json.decodeObject
      ->Or_error.fromOption_s("Failed to decode GraphState object JSON")
      ->Or_error.flatMap(dict => {
        let getValue = (key, reader) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(reader)
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(1) => {
            let nodes = getValue("nodes", Array.fromJson(_, GraphNode.Stable.V1.fromJson))
            let links = getValue("links", Array.fromJson(_, GraphLink.Stable.V1.fromJson))
            (nodes, links)
            ->Or_error.both
            ->Or_error.map(((nodes, links)) => {
              nodes: nodes,
              links: links,
              selection: Selection.empty,
            })
          }
        | Or_error.Ok(v) => Or_error.error_ss(["Unknown ConstructorData version ", Int.toString(v)])
        | Or_error.Err(e) => Or_error.error(e)
        }
      })
  }
}
