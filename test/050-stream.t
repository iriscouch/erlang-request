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

    , etap:plan(16)
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
    , etap:is(Body, <<"GET\r\n/normal\r\nGoodbye\r\n">>, "Got the streaming response")
    .

test_streaming_body() -> ok
    , Opts = {[ {url,?HOST++"/streaming/post"}, {onResponse,true} ]}
    , {Res, Body} = request:post(Opts)
    , etap:isnt(Res, error, "Stream server response to POST")
    , etap:is(Res:headers("transfer-encoding"), "chunked", "Streamer is a chunked encoding")
    , etap:is(Body(), <<"POST\r\n">>, "First streamer line")
    , etap:is(Body(), <<"/streaming/post\r\n">>, "Second streamer line")
    , etap:is(Body(), <<"Goodbye\r\n">>, "Third streamer line")
    , etap:is(Body(), 'end', "Streamer end")

    , Me = self()
    , Check_body = fun(Resp, Get_body) -> ok
        , etap:isnt(Resp, error, "Good response for async onRequest")
        , etap:is(Res:headers("transfer-encoding"), "chunked", "Async stream is a chunked encoding")
        , etap:is(Get_body(), <<"POST\r\n">>, "First async streamer line")
        , etap:is(Get_body(), <<"/streaming/async\r\n">>, "Second async streamer line")
        , etap:is(Get_body(), <<"Goodbye\r\n">>, "Third async streamer line")
        , etap:is(Get_body(), 'end', "Async streamer end")
        , Me ! done
        end
    , Pid = request:request({[ {method,post}, {url,?HOST++"/streaming/async"}, {onResponse,Check_body} ]})
    , etap:ok(is_pid(Pid), "Request with onResponse is async")
    , Done = receive
        done       -> true
        after 1000 -> false
        end
    , etap:ok(Done, "Body check finished")
    .

%dot(Obj, Key) -> request:dot(Obj, Key).

% vim: sts=4 sw=4 et
