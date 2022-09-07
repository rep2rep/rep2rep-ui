// This module provides the static settings for the RPC server.
// The require function provided here should be used over the one in the
// Rpc module because it already has the correct RPC server as the first
// parameter, meaning you don't need to remember it.
// That is, use
//     Rpc_service.require("foo", Foo.t_rpc, Bar.t_rpc)
// rather than
//     Rpc.require (Rpc_service.t, "foo", Foo.t_rpc, Bar.t_rpc)

// let t = Rpc.create(~host="rep2rep.cl.cam.ac.uk", ~port=80, ~path=Some("/api"))
let t = Rpc.create(~host="127.0.0.1", ~port=12345, ~path=None)
let require = (name, intype, outtype, data) => Rpc.require(t, name, intype, outtype, data)
