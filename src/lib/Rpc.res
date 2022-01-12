exception RpcError

type data = Js.TypedArray2.Uint8Array.t

module Datatype = {
  module Either2 = {
    type t<'a, 'b> =
      | FST('a)
      | SND('b)
  }

  module Either3 = {
    type t<'a, 'b, 'c> =
      | FST('a)
      | SND('b)
      | THD('c)
  }

  module Either4 = {
    type t<'a, 'b, 'c, 'd> =
      | FST('a)
      | SND('b)
      | THD('c)
      | FOR('d)
  }
  module Either5 = {
    type t<'a, 'b, 'c, 'd, 'e> =
      | FST('a)
      | SND('b)
      | THD('c)
      | FOR('d)
      | FIF('e)
  }

  module Either6 = {
    type t<'a, 'b, 'c, 'd, 'e, 'f> =
      | FST('a)
      | SND('b)
      | THD('c)
      | FOR('d)
      | FIF('e)
      | SIX('f)
  }

  type t<'a> = {
    reader: data => 'a,
    writer: 'a => data,
    name: string,
  }

  let read = (t, d) => t.reader(d)
  let write = (t, d) => t.writer(d)
  let name = t => t.name

  let convert = (t, new_name, read_conv, write_conv) => {
    reader: bytes => read_conv(read(t, bytes)),
    writer: x => write(t, write_conv(x)),
    name: new_name,
  }

  let alias = (t, new_name) => {...t, name: new_name}

  let recur = f => {
    reader: bytes => f().reader(bytes),
    writer: x => f().writer(x),
    name: "<rec>",
  }

  let ofArray = Js.TypedArray2.Uint8Array.make
  let empty = ofArray([])

  let unit_ = {
    reader: _ => (),
    writer: () => empty,
    name: "unit",
  }

  let bool_ = {
    reader: w =>
      switch Js.TypedArray2.Uint8Array.unsafe_get(w, 0) {
      | 1 => true
      | 0 => false
      | _ => raise(RpcError)
      },
    writer: b =>
      switch b {
      | true => ofArray([1])
      | false => ofArray([0])
      },
    name: "bool",
  }

  let int_ = {
    // JS "ints" are floats, so this might be pretty dangerous!
    let packInt = i => {
      let shift = (i, s) => mod(i / Js.Math.pow_int(~base=2, ~exp=s), 256)
      let b1 = shift(i, 0)
      let b2 = shift(i, 8)
      let b3 = shift(i, 16)
      let b4 = shift(i, 24)
      let b5 = shift(i, 32)
      let b6 = shift(i, 40)
      let b7 = shift(i, 48)
      let b8 = shift(i, 56)
      ofArray([b8, b7, b6, b5, b4, b3, b2, b1])
    }
    let unpackInt = b => {
      let unshift = (i, s) => i * Js.Math.pow_int(~base=2, ~exp=s)
      let get = Js.TypedArray2.Uint8Array.unsafe_get
      let b1 = get(b, 7)
      let b2 = get(b, 6)
      let b3 = get(b, 5)
      let b4 = get(b, 4)
      let b5 = get(b, 3)
      let b6 = get(b, 2)
      let b7 = get(b, 1)
      let b8 = get(b, 0)
      b1 +
      unshift(b2, 8) +
      unshift(b3, 16) +
      unshift(b4, 24) +
      unshift(b5, 32) +
      unshift(b6, 40) +
      unshift(b7, 48) +
      unshift(b8, 56)
    }
    {
      reader: unpackInt,
      writer: packInt,
      name: "int",
    }
  }

  let float_ = {
    reader: buffer => {
      Js.TypedArray2.Uint8Array.reverseInPlace(buffer)
      ->Js.TypedArray2.Uint8Array.buffer
      ->Js.TypedArray2.Float64Array.fromBuffer
      ->Js.TypedArray2.Float64Array.unsafe_get(0)
    },
    writer: f => {
      let b = Js.TypedArray2.Float64Array.fromLength(1)
      b->Js.TypedArray2.Float64Array.setArray([f])
      b
      ->Js.TypedArray2.Float64Array.buffer
      ->Js.TypedArray2.Uint8Array.fromBuffer
      ->Js.TypedArray2.Uint8Array.reverseInPlace
    },
    name: "float",
  }

  let string_ = {
    reader: buffer => {
      let arr = []
      Js.TypedArray2.Uint8Array.forEach(buffer, (. x) => {
        let _ = Js.Array2.push(arr, Js.String2.fromCharCode(x))
      })
      Js.String2.concatMany("", arr)
    },
    writer: s =>
      Js.String2.split(s, "")
      ->Js.Array2.map(c => Js.String2.charCodeAt(c, 0)->Belt.Float.toInt)
      ->ofArray,
    name: "string",
  }

  let getBytes = (bytes, start, len) =>
    Js.TypedArray2.Uint8Array.subarray(~start, ~end_=start + len, bytes)

  @send external setOffset: (data, data, int) => unit = "set"

  let concat = bytes => {
    let length = Js.Array2.reduce(bytes, (len, ba) => len + Js.TypedArray2.Uint8Array.length(ba), 0)
    let newBytes = Js.TypedArray2.Uint8Array.fromLength(length)
    let offset = ref(0)
    bytes->Js.Array2.forEach(b => {
      setOffset(newBytes, b, offset.contents)
      offset := offset.contents + Js.TypedArray2.Uint8Array.length(b)
    })
    newBytes
  }

  let tupleN_ = length => {
    let readTuple = bytes => {
      let rec readLens = (n, offset, answer) => {
        if n == 0 {
          answer
        } else {
          let len = read(int_, getBytes(bytes, offset, 8))
          readLens(n - 1, offset + 8, Js.Array2.concat(answer, [len]))
        }
      }
      let rec readValues = (lens, offset, answer) => {
        let len = Js.Array2.shift(lens)
        switch len {
        | None => answer
        | Some(len) => {
            let bytes = getBytes(bytes, offset, len)
            readValues(lens, offset + len, Js.Array2.concat(answer, [bytes]))
          }
        }
      }
      let lens = readLens(length, 0, [])
      readValues(lens, length * 8, [])
    }
    let writeTuple = byteArr => {
      let lens =
        byteArr->Js.Array2.map(bytes => Js.TypedArray2.Uint8Array.length(bytes) |> write(int_))
      concat(Js.Array2.concat(lens, byteArr))
    }
    {
      reader: readTuple,
      writer: writeTuple,
      name: "tuple",
    }
  }

  let tuple2_ = (at, bt) =>
    tupleN_(2)->convert(
      Js.String2.concatMany("", ["(", name(at), ", ", name(bt), ")"]),
      arr =>
        switch arr {
        | [a, b] => (read(at, a), read(bt, b))
        | _ => raise(RpcError)
        },
      ((a, b)) => [write(at, a), write(bt, b)],
    )

  let tuple3_ = (at, bt, ct) =>
    tupleN_(3)->convert(
      Js.String2.concatMany("", ["(", name(at), ", ", name(bt), ", ", name(ct), ")"]),
      arr =>
        switch arr {
        | [a, b, c] => (read(at, a), read(bt, b), read(ct, c))
        | _ => raise(RpcError)
        },
      ((a, b, c)) => [write(at, a), write(bt, b), write(ct, c)],
    )

  let tuple4_ = (at, bt, ct, dt) =>
    tupleN_(4)->convert(
      Js.String2.concatMany(
        "",
        ["(", name(at), ", ", name(bt), ", ", name(ct), ", ", name(dt), ")"],
      ),
      arr =>
        switch arr {
        | [a, b, c, d] => (read(at, a), read(bt, b), read(ct, c), read(dt, d))
        | _ => raise(RpcError)
        },
      ((a, b, c, d)) => [write(at, a), write(bt, b), write(ct, c), write(dt, d)],
    )

  let tuple5_ = (at, bt, ct, dt, et) =>
    tupleN_(5)->convert(
      Js.String2.concatMany(
        "",
        ["(", name(at), ", ", name(bt), ", ", name(ct), ", ", name(dt), ", ", name(et), ")"],
      ),
      arr =>
        switch arr {
        | [a, b, c, d, e] => (read(at, a), read(bt, b), read(ct, c), read(dt, d), read(et, e))
        | _ => raise(RpcError)
        },
      ((a, b, c, d, e)) => [write(at, a), write(bt, b), write(ct, c), write(dt, d), write(et, e)],
    )

  let tuple6_ = (at, bt, ct, dt, et, ft) =>
    tupleN_(6)->convert(
      Js.String2.concatMany(
        "",
        [
          "(",
          name(at),
          ", ",
          name(bt),
          ", ",
          name(ct),
          ", ",
          name(dt),
          ", ",
          name(et),
          ", ",
          name(ft),
          ")",
        ],
      ),
      arr =>
        switch arr {
        | [a, b, c, d, e, f] => (
            read(at, a),
            read(bt, b),
            read(ct, c),
            read(dt, d),
            read(et, e),
            read(ft, f),
          )
        | _ => raise(RpcError)
        },
      ((a, b, c, d, e, f)) => [
        write(at, a),
        write(bt, b),
        write(ct, c),
        write(dt, d),
        write(et, e),
        write(ft, f),
      ],
    )

  let eitherN_ = {
    let readEither = bytes => {
      let len = Js.TypedArray2.Uint8Array.length(bytes) - 1
      (Js.TypedArray2.Uint8Array.unsafe_get(bytes, 0), getBytes(bytes, 1, len))
    }
    let writeEither = ((idx, data)) => concat([ofArray([idx]), data])
    {
      reader: readEither,
      writer: writeEither,
      name: "either",
    }
  }

  let either2_ = (at, bt) =>
    eitherN_->convert(
      Js.String2.concatMany("Rpc.Datatype.Either2.t", ["<", name(at), ", ", name(bt), ">"]),
      ((idx, data)) =>
        switch idx {
        | 0 => Either2.FST(read(at, data))
        | 1 => Either2.SND(read(bt, data))
        | _ => raise(RpcError)
        },
      value =>
        switch value {
        | Either2.FST(a) => (0, write(at, a))
        | Either2.SND(b) => (1, write(bt, b))
        },
    )

  let either3_ = (at, bt, ct) =>
    eitherN_->convert(
      Js.String2.concatMany(
        "Rpc.Datatype.Either3.t",
        ["<", name(at), ", ", name(bt), ", ", name(ct), ">"],
      ),
      ((idx, data)) =>
        switch idx {
        | 0 => Either3.FST(read(at, data))
        | 1 => Either3.SND(read(bt, data))
        | 2 => Either3.THD(read(ct, data))
        | _ => raise(RpcError)
        },
      value =>
        switch value {
        | Either3.FST(a) => (0, write(at, a))
        | Either3.SND(b) => (1, write(bt, b))
        | Either3.THD(c) => (2, write(ct, c))
        },
    )

  let either4_ = (at, bt, ct, dt) =>
    eitherN_->convert(
      Js.String2.concatMany(
        "Rpc.Datatype.Either4.t",
        ["<", name(at), ", ", name(bt), ", ", name(ct), ", ", name(dt), ">"],
      ),
      ((idx, data)) =>
        switch idx {
        | 0 => Either4.FST(read(at, data))
        | 1 => Either4.SND(read(bt, data))
        | 2 => Either4.THD(read(ct, data))
        | 3 => Either4.FOR(read(dt, data))
        | _ => raise(RpcError)
        },
      value =>
        switch value {
        | Either4.FST(a) => (0, write(at, a))
        | Either4.SND(b) => (1, write(bt, b))
        | Either4.THD(c) => (2, write(ct, c))
        | Either4.FOR(d) => (3, write(dt, d))
        },
    )

  let either5_ = (at, bt, ct, dt, et) =>
    eitherN_->convert(
      Js.String2.concatMany(
        "Rpc.Datatype.Either5.t",
        ["<", name(at), ", ", name(bt), ", ", name(ct), ", ", name(dt), ", ", name(et), ">"],
      ),
      ((idx, data)) =>
        switch idx {
        | 0 => Either5.FST(read(at, data))
        | 1 => Either5.SND(read(bt, data))
        | 2 => Either5.THD(read(ct, data))
        | 3 => Either5.FOR(read(dt, data))
        | 4 => Either5.FIF(read(et, data))
        | _ => raise(RpcError)
        },
      value =>
        switch value {
        | Either5.FST(a) => (0, write(at, a))
        | Either5.SND(b) => (1, write(bt, b))
        | Either5.THD(c) => (2, write(ct, c))
        | Either5.FOR(d) => (3, write(dt, d))
        | Either5.FIF(e) => (4, write(et, e))
        },
    )

  let either6_ = (at, bt, ct, dt, et, ft) =>
    eitherN_->convert(
      Js.String2.concatMany(
        "Rpc.Datatype.Either6.t",
        [
          "<",
          name(at),
          ", ",
          name(bt),
          ", ",
          name(ct),
          ", ",
          name(dt),
          ", ",
          name(et),
          ", ",
          name(ft),
          ">",
        ],
      ),
      ((idx, data)) =>
        switch idx {
        | 0 => Either6.FST(read(at, data))
        | 1 => Either6.SND(read(bt, data))
        | 2 => Either6.THD(read(ct, data))
        | 3 => Either6.FOR(read(dt, data))
        | 4 => Either6.FIF(read(et, data))
        | 5 => Either6.SIX(read(ft, data))
        | _ => raise(RpcError)
        },
      value =>
        switch value {
        | Either6.FST(a) => (0, write(at, a))
        | Either6.SND(b) => (1, write(bt, b))
        | Either6.THD(c) => (2, write(ct, c))
        | Either6.FOR(d) => (3, write(dt, d))
        | Either6.FIF(e) => (4, write(et, e))
        | Either6.SIX(f) => (5, write(ft, f))
        },
    )

  let option_ = at => {
    let {reader: r, writer: w, name: _} = either2_(at, unit_)
    {
      reader: bytes =>
        switch r(bytes) {
        | Either2.FST(v) => Some(v)
        | Either2.SND() => None
        },
      writer: opt =>
        switch opt {
        | Some(v) => w(Either2.FST(v))
        | None => w(Either2.SND())
        },
      name: "option<" ++ name(at) ++ ">",
    }
  }

  let array_ = at => {
    // Leading int for overall length,
    // then each element we lead with byte length
    // then the byte encoding of the element.
    let arrayReader = bytes => {
      let length = read(int_, getBytes(bytes, 0, 8))
      let ans = []
      let rec f = (remaining, offset) => {
        if remaining == 0 {
          ()
        } else {
          let el_len = read(int_, getBytes(bytes, offset, 8))
          let x = read(at, getBytes(bytes, offset + 8, el_len))
          let _ = Js.Array2.push(ans, x)
          f(remaining - 1, offset + 8 + el_len)
        }
      }
      f(length, 8)
      ans
    }
    let arrayWriter = xs => {
      let len_bytes = write(int_, Belt.Array.length(xs))
      let encoded_elements = Belt.Array.map(xs, x => {
        let b = write(at, x)
        let len_b = write(int_, Js.TypedArray2.Uint8Array.length(b))
        concat([len_b, b])
      })
      concat(Belt.Array.concat([len_bytes], encoded_elements))
    }
    {
      reader: arrayReader,
      writer: arrayWriter,
      name: "array<" ++ name(at) ++ ">",
    }
  }

  let list_ = at => {
    let {reader: r, writer: w, name: _} = array_(at)
    {
      reader: bytes => r(bytes)->Belt.List.fromArray,
      writer: xs => Belt.List.toArray(xs)->w,
      name: "list<" ++ name(at) ++ ">",
    }
  }
}

module Response = {
  type t<'a> = Promise.t<option<'a>>

  let upon = (t, f) =>
    t
    ->Promise.thenResolve(t =>
      switch t {
      | Some(t) => f(t)
      | None => ()
      }
    )
    ->ignore

  let map = (t, f) => t->Promise.thenResolve(t => t->Belt.Option.map(f))

  let flatMap = (t, f) =>
    t->Promise.then(t =>
      switch t {
      | Some(t) => f(t)
      | None => Promise.resolve(None)
      }
    )
}

type t = {
  host: string,
  port: int,
}

let create = (host, port) => {host: host, port: port}

let require = (service, endpoint, param_type, return_type) => {
  param => {
    let param_bytes = Datatype.write(param_type, param)
    Fetch.fetch(
      service.host ++ ":" ++ Belt.Int.toString(service.port) ++ "/" ++ endpoint,
      ~method=POST,
      ~body=param_bytes,
      (),
    )->Promise.then(response =>
      if response.ok {
        Fetch.Response.arrayBuffer(response)->Promise.thenResolve(buff => {
          Some(Datatype.read(return_type, buff))
        })
      } else {
        Promise.resolve(None)
      }
    )
  }
}
