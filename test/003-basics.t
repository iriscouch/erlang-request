#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 12345).
-define(HOST, "http://localhost:12345").

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
    , {Res, Body} = request:get(?HOST)
    , etap:isnt(Res, error, "Fetching from server works")
    , io:format("Res: ~p\n", [Res])
    .

% vim: sts=4 sw=4 et
