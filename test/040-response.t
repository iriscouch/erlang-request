#! /usr/bin/env escript

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")

    , etap:plan(11)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_creation()
    , test_values()
    .

test_creation() -> ok
    , Res = request:new_response(socket, {"1.1", 201, "Created"}, [], "")
    , etap:ok(is_tuple(Res), "Built a response module (tuple)")
    , etap:is(element(1, Res), request_response, "Got a request_response module")
    .

test_values() -> ok
    , {Hdrs, Body} = json("{\"ok\":true}")
    , Res = request:new_response(a_socket, {"1.1", 200, "OK"}, Hdrs, Body)

    , etap:is(Res:socket(), a_socket, "Response.socket")
    , etap:is(Res:httpVersion(), "1.1", "Response httpVersion is right")
    , etap:is(Res:statusCode(), 200, "Response statusCode is right")
    , etap:is(Res:message(), "OK", "Response message is right")

    , {Headers} = Res:headers()
    , etap:ok(is_list(Headers), "Headers is an ejson object")

    , etap:is(Res:headers("content-type"), "application/json", "Response can lookup a header")
    , etap:is(Res:headers("CoNTEnT-LENGTH"), "11", "Response header is case-insensitive")
    , etap:is(Res:headers("X-Wasnt-there"), undefined, "Missing response header -> undefined")

    , etap:is(Res:body(), "{\"ok\":true}", "Response body is right")
    .

json(Body) -> ok
    , Length = integer_to_list(length(Body))
    , Headers = [{"content-type","application/json"}, {"content-length", Length}]
    , {Headers, Body}
    .

% vim: sts=4 sw=4 et
