#! /usr/bin/env escript

-define(COUCH, "http://localhost:5984").
-define(TIMEOUT, 3000).

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")

    , etap:plan(7)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_api()
    , test_call_styles()
    .

test_api() -> ok
    , Request = request:api()
    , etap:ok(is_function(Request), "api() returns an API function")
    .

test_call_styles() -> ok
    , Request = request:api()
    , ARequest = request:api(async)
    , {Waiter, Handler} = handler(ok)

    , Res1 = request:req(?COUCH)
    , etap:ok(is_tuple(Res1), "request:req(url) returns a tuple")

    , Res2 = Request(?COUCH)
    , etap:ok(is_tuple(Res2), "Request(url) returns a tuple")

    , Pid1 = request:req(?COUCH, Handler)
    , etap:ok(is_pid(Pid1), "request:req(url, callback) returns a PID")
    , Waiter(Pid1)

    , Pid2 = ARequest(?COUCH, Handler)
    , etap:ok(is_pid(Pid2), "Request(url, callback) returns a PID")
    , Waiter(Pid2)
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
