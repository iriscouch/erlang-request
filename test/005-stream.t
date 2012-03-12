#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 12345).
-define(HOST, "http://localhost:12345").

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")
    , code:add_pathz("deps/ejson/ebin")

    , request:start()
    , http_server:run(stream, ?PORT)

    , etap:plan(1)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_normal()
    , test_streaming_body()
    .

test_normal() -> ok
    , {Res, Body} = request:get(?HOST ++ "/normal")
    , etap:isnt(Res, error, "Stream server response")
    , etap:is(Body, "GET\r\n/normal\r\nGoodbye\r\n", "Got the streaming response")
    .

test_streaming_body() -> ok
    .

dot(Obj, Key) -> request:dot(Obj, Key).
dot(Obj, Key, Val) -> request:dot(Obj, Key, Val).

% vim: sts=4 sw=4 et
