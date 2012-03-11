#! /usr/bin/env escript

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")

    , etap:plan(9)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_creation()
    , test_values()
    .

test_creation() -> ok
    , Res = request:new_response(httpc())
    , etap:ok(is_tuple(Res), "Built a response module (tuple)")
    , etap:is(element(1, Res), request_response, "Got a request_response module")
    .

test_values() -> ok
    , {Hdrs, Body} = json("{\"ok\":true}")
    , Httpc = httpc(200, Hdrs, Body)
    , Res = request:new_response(Httpc)

    , etap:is(Res:httpVersion(), "1.1", "Response httpVersion is right")
    , etap:is(Res:statusCode(), 200, "Response statusCode is right")
    , etap:is(Res:message(), "OK", "Response message is right")

    , {Headers} = Res:headers()
    , etap:ok(is_list(Headers), "Headers is an ejson object")

    , etap:is(Res:headers("content-type"), "application/json", "Response can lookup a header")
    , etap:is(Res:headers("CoNTEnT-LENGTH"), "11", "Response header is case-insensitive")
    , etap:is(Res:headers("X-Wasnt-there"), undefined, "Missing response header -> undefined")
    .

httpc() -> ok
    , httpc("Hello, world")
    .

httpc(Body) -> ok
    , httpc(200, Body)
    .

httpc(Status, Body) -> ok
    , httpc(Status, [], Body)
    .

httpc(Status, Headers, Body) -> ok
    , httpc("HTTP/1.1", Status, Headers, Body)
    .

httpc(Version, Status, Headers, Body) -> ok
    , Head = {Version, Status, "OK"}
    , {Head, Headers, Body}
    .

json(Body) -> ok
    , Length = integer_to_list(length(Body))
    , Headers = [{"content-type","application/json"}, {"content-length", Length}]
    , {Headers, Body}
    .

% vim: sts=4 sw=4 et
