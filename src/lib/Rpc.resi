// Note that this MUST be kept in sync with src/util/rpc.sml in the rep2rep repo.

exception RpcError

type data

module Datatype: {
  type t<'a>

  module Either2: {
    type t<'a, 'b> =
      | FST('a)
      | SND('b)
  }

  module Either3: {
    type t<'a, 'b, 'c> =
      | FST('a)
      | SND('b)
      | THD('c)
  }

  module Either4: {
    type t<'a, 'b, 'c, 'd> =
      | FST('a)
      | SND('b)
      | THD('c)
      | FOR('d)
  }
  module Either5: {
    type t<'a, 'b, 'c, 'd, 'e> =
      | FST('a)
      | SND('b)
      | THD('c)
      | FOR('d)
      | FIF('e)
  }

  module Either6: {
    type t<'a, 'b, 'c, 'd, 'e, 'f> =
      | FST('a)
      | SND('b)
      | THD('c)
      | FOR('d)
      | FIF('e)
      | SIX('f)
  }

  let read: (t<'a>, data) => 'a
  let write: (t<'a>, 'a) => data
  let name: t<'a> => string

  let convert: (t<'a>, string, 'a => 'b, 'b => 'a) => t<'b>
  let alias: (t<'a>, string) => t<'a>

  let recur: (unit => t<'a>) => t<'a>

  let unit_: t<unit>
  let bool_: t<bool>
  let int_: t<int>
  let float_: t<float>
  let string_: t<string>
  let option_: t<'a> => t<option<'a>>
  let array_: t<'a> => t<array<'a>>
  let list_: t<'a> => t<list<'a>>

  let tuple2_: (t<'a>, t<'b>) => t<('a, 'b)>
  let tuple3_: (t<'a>, t<'b>, t<'c>) => t<('a, 'b, 'c)>
  let tuple4_: (t<'a>, t<'b>, t<'c>, t<'d>) => t<('a, 'b, 'c, 'd)>
  let tuple5_: (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>) => t<('a, 'b, 'c, 'd, 'e)>
  let tuple6_: (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>) => t<('a, 'b, 'c, 'd, 'e, 'f)>
  let tupleN_: int => t<array<data>>

  let either2_: (t<'a>, t<'b>) => t<Either2.t<'a, 'b>>
  let either3_: (t<'a>, t<'b>, t<'c>) => t<Either3.t<'a, 'b, 'c>>
  let either4_: (t<'a>, t<'b>, t<'c>, t<'d>) => t<Either4.t<'a, 'b, 'c, 'd>>
  let either5_: (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>) => t<Either5.t<'a, 'b, 'c, 'd, 'e>>
  let either6_: (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>) => t<Either6.t<'a, 'b, 'c, 'd, 'e, 'f>>
  let eitherN_: t<(int, data)>
}

module Response: {
  type t<'a> = Promise.t<option<'a>>

  let upon: (t<'a>, 'a => unit) => unit
  let map: (t<'a>, 'a => 'b) => t<'b>
  let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
}

type t

let create: (string, int) => t
let require: (t, string, Datatype.t<'a>, Datatype.t<'b>, 'a) => Response.t<'b>
