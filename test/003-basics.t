#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 12345).
-define(HOST, "http://localhost:12345").
%-define(HOST, "http://localhost:5984").

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")

    , request:start()
    , http_server:echo(?PORT)

    , etap:plan(5)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_methods()
    .

test_methods() -> ok
    , Functions = [req, get, put, post, delete]
    , lists:foreach(fun(Func) -> ok
        , {Res, Body} = request:Func(?HOST)
        , etap:isnt(Res, error, "Request using request:" ++ atom_to_list(Func))
        end, Functions)
    .

% vim: sts=4 sw=4 et
