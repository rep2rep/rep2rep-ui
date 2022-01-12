// This module provides the static settings for the RPC server.
// The require function provided here should be used over the one in the
// Rpc module because it already has the correct RPC server as the first
// parameter, meaning you don't need to remember it.
// That is, use
//     Rpc_service.require("foo", Foo.t_rpc, Bar.t_rpc)
// rather than
//     Rpc.require (Rpc_service.t, "foo", Foo.t_rpc, Bar.t_rpc)

let t = Rpc.create("192.168.1.40", 12345)
let require = (name, intype, outtype, data) => Rpc.require(t, name, intype, outtype, data)
