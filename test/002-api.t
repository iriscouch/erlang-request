#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 12345).
-define(HOST, "http://localhost:12345").

-import(request, [request/1, request/2]).

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")
    , code:add_pathz("deps/ejson/ebin")

    , request:start()
    , http_server:run(noop, ?PORT)

    , etap:plan(18)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_call_styles()
    , test_shortcuts()
    .

test_call_styles() -> ok
    , {Waiter, Handler} = handler(ok)

    , Res1 = request:request(?HOST)
    , etap:ok(is_tuple(Res1), "request:request(url) returns a tuple")

    , Res2 = request(?HOST)
    , etap:ok(is_tuple(Res2), "request(url) returns a tuple")

    , Pid1 = request:request(?HOST, Handler)
    , etap:ok(is_pid(Pid1), "request:request(url, callback) returns a PID")
    , Waiter(Pid1)

    , Pid2 = request(?HOST, Handler)
    , etap:ok(is_pid(Pid2), "request(url, callback) returns a PID")
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
        , etap:ok(true, "request callback runs")
        , Waiter_pid ! {self(), ok}
        end
    , {Waiter, Handler}
    .

% vim: sts=4 sw=4 et
