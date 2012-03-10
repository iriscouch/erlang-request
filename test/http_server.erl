% #! /usr/bin/env escript

-module(http_server).

-export([noop/1]).

-define(TIMEOUT, 3000).

noop(Port) -> ok
    , Noop = fun(Sock, _Method, _Path, Version, _Headers) -> ok
        , reply(Sock, Version, "")
        end
    , listen(Port, Noop)
    .

reply(Sock, Version, Body) when is_list(Body) -> ok
    , reply(Sock, Version, list_to_binary(Body))
    ;

reply(Sock, Version, Body) -> ok
    , StatusCode = 200
    , StatusMessage = "OK"
    , ResponseVersion = case Version
        of {1,1} -> "HTTP/1.1"
        ; _      -> "HTTP/1.0"
        end

    , Length = size(Body)
    , Headers = [["Content-Length: ", integer_to_list(Length)]]

    , Response =
        [ ResponseVersion, " ", integer_to_list(StatusCode), " ", StatusMessage
        , "\r\n"
        , [ [X, "\r\n"] || X <- Headers ]
        , "\r\n"
        , Body
        ]

    , io:format("Response:\n~p\n", [Response])
    , gen_tcp:send(Sock, Response)
    .

listen(Port, Handler) -> ok
    , Pid = spawn_link(fun() -> listen(child, Port, Handler) end)
    , Pid
    .

listen(child, Port, Handler) -> ok
    , {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active,false}, {reuseaddr,true}])
    , acceptor(ListenSocket, Handler)
    .

acceptor(ListenSocket, Handler) -> ok
    , case gen_tcp:accept(ListenSocket)
        of {ok, Sock} -> ok
            , Pid = spawn(fun() -> handle_http(Sock, Handler) end)
            , chown(Sock, Pid)
            , acceptor(ListenSocket, Handler)
        ; NotOk -> ok
            , throw({http_server_error, NotOk})
        end
    .

handle_http(Sock, Handler) -> ok
    , receive
        got_permission -> ok
            , handle_http(ready, Sock, Handler)
        after ?TIMEOUT -> ok
            , throw({http_server_error, timeout})
        end
    .

handle_http(ready, Sock, Handler) -> ok
    , inet:setopts(Sock, [binary, {packet,http}, {active,false}])
    , get_headers(Sock, Handler, [])
    .

get_headers(Sock, Handler, Headers) -> ok
    , inet:setopts(Sock, [{active,once}])
    , receive
        {_Type, Sock, {http_header, _Length, Key, undefined, Value}} -> ok
            , get_headers(Sock, Handler, [{header, {Key,Value}} | Headers])
        ; {_Type, Sock, {http_request, Method, Path, Version}} -> ok
            , get_headers(Sock, Handler, [{http_request, Method, Path, Version} | Headers])
        ; {_Type, Sock, http_eoh} -> ok
            , Packets = lists:reverse(Headers)
            , case lists:keytake(http_request, 1, Packets)
                of false -> ok
                    , throw({http_server_error, no_http_request})
                ; {value, RequestPacket, HeaderPackets} -> ok
                    , {http_request, Method, Path, Version} = RequestPacket
                    , RequestHeaders = [ {to_atom(Key), Val} || {header, {Key, Val}} <- HeaderPackets ]

                    % Done. Call the handler.
                    , Handler(Sock, Method, Path, Version, RequestHeaders)
                end
        ; Msg -> ok
            , throw({http_server_error, Msg})
        after ?TIMEOUT -> ok
            , throw({http_server_error, timeout})
        end
    .

chown(Socket, Pid) -> ok
    , gen_tcp:controlling_process(Socket, Pid)
    , Pid ! got_permission
    .

to_atom(X) when is_list(X) -> erlang:list_to_existing_atom(X);
to_atom(X) when is_binary(X) -> to_atom(binary_to_list(X));
to_atom(X) when is_atom(X) -> X.

% vim: sts=4 sw=4 et
