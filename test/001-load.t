#!/usr/bin/env escript

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")

    , etap:plan(3)
    , etap:loaded_ok(request, "Load 'request' module")

    , Exports = request:module_info(exports)
    , etap:ok(exported(Exports, {request,1}), "Exported: request:request/1")
    , etap:ok(exported(Exports, {request,2}), "Exported: request:request/2")
    , etap:end_tests()
    .

exported(Exports, Func) -> ok
    , lists:any(fun(Export) -> Export =:= Func end, Exports)
    .

% vim: sts=4 sw=4 et
