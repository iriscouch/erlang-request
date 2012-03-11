#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 12345).
-define(HOST, "http://localhost:12345").

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")

    , request:start()
    , http_server:echo(?PORT)

    , etap:plan(19)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_methods()
    , test_response()
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

% vim: sts=4 sw=4 et
