#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 12345).
-define(HOST, "http://localhost:22345").

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")

    , request:start()
    , http_server:noop(?PORT)

    , etap:plan(1)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_get()
    .

test_get() -> ok
    , Res = request:get(?HOST)
    , etap:ok(is_tuple(Res), "Fetching from the server works")
    .

% vim: sts=4 sw=4 et
