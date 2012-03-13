#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 12345).
-define(HOST, "http://localhost:12345").

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")
    , code:add_pathz("deps/ejson/ebin")

    , request:start()
    , http_server:run(noop, ?PORT)

    , etap:plan(20)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_api()
    , test_call_styles()
    , test_shortcuts()
    .

test_api() -> ok
    , Request = request:api()
    , etap:ok(is_function(Request), "api() returns an API function")

    , Request_async = request:api(async)
    , etap:ok(is_function(Request_async), "api(async) returns an API function")
    .

test_call_styles() -> ok
    , Request = request:api()
    , ARequest = request:api(async)
    , {Waiter, Handler} = handler(ok)

    , Res1 = request:req(?HOST)
    , etap:ok(is_tuple(Res1), "request:req(url) returns a tuple")

    , Res2 = Request(?HOST)
    , etap:ok(is_tuple(Res2), "Request(url) returns a tuple")

    , Pid1 = request:req(?HOST, Handler)
    , etap:ok(is_pid(Pid1), "request:req(url, callback) returns a PID")
    , Waiter(Pid1)

    , Pid2 = ARequest(?HOST, Handler)
    , etap:ok(is_pid(Pid2), "Request(url, callback) returns a PID")
    , Waiter(Pid2)
    .

test_shortcuts() -> ok
    , {Waiter, Handler} = handler(ok)
    , Url = ?HOST ++ "/db"

    , Get = request:get(Url)
    , etap:ok(is_tuple(Get), "request:get() runs")

    , Put = request:put(Url)
    , etap:ok(is_tuple(Put), "request.put() runs")

    , Post = request:post(Url)
    , etap:ok(is_tuple(Post), "request.post() runs")

    , Delete = request:delete(Url)
    , etap:ok(is_tuple(Delete), "request.delete() runs")

    , Pid1 = request:get(Url, Handler)
    , etap:ok(is_pid(Pid1), "request:get(url, callback) returns a PID")
    , Waiter(Pid1)

    , Pid2 = request:put(Url, Handler)
    , etap:ok(is_pid(Pid2), "request:put(url, callback) returns a PID")
    , Waiter(Pid2)

    , Pid3 = request:post(Url, Handler)
    , etap:ok(is_pid(Pid3), "request:post(url, callback) returns a PID")
    , Waiter(Pid3)

    , Pid4 = request:delete(Url, Handler)
    , etap:ok(is_pid(Pid4), "request:delete(url, callback) returns a PID")
    , Waiter(Pid4)

    .

handler(ok) -> ok
    , Waiter_pid = self()
    , Waiter = fun(Handler_pid) -> ok
        , receive
            {Handler_pid, ok} -> ok
            after ?TIMEOUT    -> throw(timeout)
            end
        end
    , Handler = fun(_Type, _Result) -> ok
        , etap:ok(true, "Request callback runs")
        , Waiter_pid ! {self(), ok}
        end
    , {Waiter, Handler}
    .

% vim: sts=4 sw=4 et
