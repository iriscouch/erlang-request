#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 12345).
-define(HOST, "http://localhost:12345").

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")
    , code:add_pathz("deps/ejson/ebin")

    , request:start()
    , http_server:run(echo, ?PORT)

    , etap:plan(46)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_methods()
    , test_response()
    , test_json()
    , test_syntax()
    .

test_methods() -> ok
    , Functions = [req, get, put, post, delete]
    , lists:foreach(fun(Func) -> ok
        , Url = ?HOST ++ "/" ++ atom_to_list(Func)
        , {Res, Body} = request:Func(Url)
        , etap:isnt(Res, error, "Request using request:" ++ atom_to_list(Func))
        end, Functions)

    , Request = request:api(async)
    , lists:foreach(fun(Func) -> ok
        , {Waiter, Handler} = handler(atom_to_list(Func))
        , Url = ?HOST ++ "/async/" ++ atom_to_list(Func)
        , Opts = case Func
            of req -> Url
            ; _ -> {[{method,Func}, {uri,Url}]}
            end
        , Pid = Request(Opts, Handler)
        , Waiter(Pid)
        end, Functions)
    .

test_response() -> ok
    , {Res, Body} = request:get({[{uri,?HOST}]})
    , etap:isnt(Res, error, "No error fetching " ++ ?HOST)

    , etap:is(Res:httpVersion(), "1.1", "res.httpVersion")
    , etap:is(Res:statusCode(), 200, "res.statusCode")

    , etap:ok(is_tuple(Res:headers()), "res.headers is ejson")
    , {Headers} = Res:headers()
    , etap:ok(is_list(Headers), "res.headers is well-formed ejson")
    , etap:is(Res:headers("Content-Type"), "application/json", "res.headers['content-type']")
    , etap:is(Res:headers("X-Not-Here"), undefined, "res.headers.not_here == undefined")

    , etap:ok(is_list(Body), "res.body is a string")
    , etap:is(Body, Res:body(), "res.body and body are the same thing")
    .

test_json() -> ok
    , {Res1, Body1} = request:get({[{uri, ?HOST ++ "/json"}, {json,true}]})
    , etap:isnt(Res1, error, "No error with json:true")
    , etap:ok(is_tuple(Body1), "Response with json:true is an ejson tuple")
    , etap:is(Res1:body(), Body1, "res.body == body for json:true")
    .

test_syntax() -> ok
    , Methods = [get, put, post, delete]
    , lists:foreach(fun(Method) -> ok
        , Meth_s = atom_to_list(Method)
        , Path = "/syntax/" ++ Meth_s
        , Sent_method = string:to_upper(Meth_s)
        , Url = ?HOST ++ Path

        , {Res, Body} = request:Method({[{uri,Url}, {json,true}]})
        , etap:isnt(Res, error, "No problem " ++ Sent_method ++ " " ++ Url)

        , etap:is(dot(Body, ".method"), list_to_binary(Sent_method), "Server saw method: " ++ Sent_method)
        , etap:is(dot(Body, ".path")  , list_to_binary(Path), "Server saw the path: " ++ Path)

        , etap:is(dot(Body, ".headers.Accept"), <<"application/json">>, "Sent accept:application/json header")
        , case Method =:= put orelse Method =:= post
            of false -> ok
            ; true -> ok
                , Sent_type = dot(Body, ".headers.Content-Type")
                , etap:is(Sent_type, <<"application/json">>, "Sent content-type:application/json for " ++ Meth_s)
            end
        end, Methods)

    , Store_methods = [put, post]
    , lists:foreach(fun(Method) -> ok
        , Meth_s = atom_to_list(Method)
        , Meth_b = list_to_binary(Meth_s)
        , Req_body = {[{<<"json_shortcut">>,true}, {<<"method">>,Meth_b}]}
        , {Res, Body} = request:Method({[{uri, ?HOST}, {json, Req_body}]})
        , etap:isnt(Res, error, "No problem with json shortcut: " ++ Meth_s)

        , Roundtrip_body = dot(Body, ".body")
        , etap:is(dot(Roundtrip_body, ".method"), Meth_b, "Server sent back method: " ++ Meth_s)
        , etap:ok(Req_body =:= Roundtrip_body, "Body round-tripped through request/response: " ++ Meth_s)
        end, Store_methods)
    .

handler(Method) -> ok
    , Waiter_pid = self()
    , Waiter = fun(Handler_pid) -> ok
        , receive
            {Handler_pid, ok} -> ok
            after ?TIMEOUT    -> throw(timeout)
            end
        end
    , Handler = fun(Type, _Result) -> ok
        , etap:isnt(Type, error, "Async handler: " ++ Method)
        , Waiter_pid ! {self(), ok}
        end
    , {Waiter, Handler}
    .

dot(Obj, Key) -> request:dot(Obj, Key).
dot(Obj, Key, Val) -> request:dot(Obj, Key, Val).

% vim: sts=4 sw=4 et
