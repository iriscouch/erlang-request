#! /usr/bin/env escript

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")

    , etap:plan(1)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_api()
    .

test_api() -> ok
    , etap:is(1, 1, "One is one")
    .
