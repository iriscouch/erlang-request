% #! /usr/bin/env escript

-module(http_server).

-export([noop/1, echo/1]).

-define(TIMEOUT, 3000).

noop(Port) -> ok
    , Noop = fun(Sock, _Method, _Path, Version, _Headers) -> ok
        , reply(Sock, Version, "Hi!")
        end
    , listen(Port, Noop)
    .

echo(Port) -> ok
    , KV = fun(Key, Val) -> ok
        , Pair =
            [ $"
            , case Key
                of _ when is_atom(Key) -> atom_to_list(Key)
                ; _                    -> Key
                end
            , $"
            , $:
            , case Val
              of _ when is_list(Val) -> [ $", Val, $" ]
              ;  _ when is_atom(Val) -> [ $", atom_to_list(Val), $" ]
              ;  _ when is_binary(Val) -> [ $", Val, $" ]
              ;  _ when is_number(Val) -> integer_to_list(Val)
              end
            ]
        , binary_to_list(iolist_to_binary(Pair))
        end

    , Echo = fun(Sock, Method, {abs_path, Path}, Version, Headers) -> ok
        , Headers_enc = string:join([ KV(Key, Val) || {Key, Val} <- Headers ], ",")
        , Resp_headers = [{"Content-Type","application/json"}]
        , Ver = case Version
            of {1,1} -> "1.1"
            ;  {1,0} -> "1.0"
            end
        , Info =
            [ {"method" , Method}
            , {"path"   , Path}
            , {"version", Ver}
            ]
        , Body =
            [ "{"
            , string:join([ KV(Key, Val) || {Key, Val} <- Info ], ",")
            , ",\"headers\":{", Headers_enc, "}"
            , "}"
            ]
        , reply(Sock, Version, Resp_headers, Body)
        end
    , listen(Port, Echo)
    .

reply(Sock, Version, Body) -> ok
    , reply(Sock, Version, [], Body)
    .

reply(Sock, Version, Headers0, Body) -> ok
    , StatusCode = 200
    , StatusMessage = "OK"
    , ResponseVersion = case Version
        of {1,1} -> "HTTP/1.1"
        ; _      -> "HTTP/1.0"
        end

    , Body_bin = iolist_to_binary(Body)
    , Length = size(Body_bin)
    , Headers = lists:keystore("Content-Length", 1, Headers0, {"Content-Length", integer_to_list(Length)})

    , Response =
        [ ResponseVersion, " ", integer_to_list(StatusCode), " ", StatusMessage
        , "\r\n"
        , [ [Key, ": ", Val, "\r\n"] || {Key, Val} <- Headers ]
        , "\r\n"
        , Body_bin
        ]

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

%to_atom(X) when is_list(X) -> erlang:list_to_existing_atom(X);
to_atom(X) when is_list(X) -> erlang:list_to_atom(X);
to_atom(X) when is_binary(X) -> to_atom(binary_to_list(X));
to_atom(X) when is_atom(X) -> X.

% vim: sts=4 sw=4 et
