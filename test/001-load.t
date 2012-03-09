#!/usr/bin/env escript

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")

    , etap:plan(2)
    , etap:loaded_ok(request, "Load 'request' module")
    , etap:can_ok(request, api)
    , etap:end_tests()
    .
