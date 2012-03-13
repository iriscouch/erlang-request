% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(request).
-export([start/0, start/1]).

-export([request/1, get/1, put/1, post/1, delete/1]).
-export([request/2, get/2, put/2, post/2, delete/2]).

-export([new_response/4]).
-export([dot/2, dot/3, normal_key/1]).

start() -> ok
    %, ssl:start()
    %, inets:start()
    .

start(_Type) -> ok
    %, ssl:start(Type)
    %, inets:start(Type)
    .

new_response(Socket, {Version_str, Status, Message}, Headers, Body) -> ok
    , Version = case Version_str
        of "HTTP/" ++ Rest -> Rest
        ; {1,1}            -> "1.1"
        ; {1,0}            -> "1.0"
        ;  _               -> to_list(Version_str)
        end
    , request_response:new(Socket, Version, Status, Message, normal_headers(Headers), Body)
    .

new_response(Response, New_data) -> ok
    , Socket  = Response:socket()
    , Version = Response:httpVersion()
    , Status  = Response:statusCode()
    , Reason  = Response:message()
    , {Headers} = Response:headers()
    , new_response(Socket, {Version, Status, Reason}, Headers, New_data)
    .

request(Url) when is_list(Url) -> ok
    , request({[ {url,Url} ]})
    ;

request(Options) -> ok
    , case dot(Options, ".onResponse")
        of Response_callback when is_function(Response_callback) -> ok
            , request(dot(Options, ".onResponse", true), Response_callback)
        ; _ -> ok
            , request(sync, Options)
        end
    .

request(sync, Options0) -> ok
    , Waiter = self()
    , Callback = fun
        (error, Reason) -> ok
            , Waiter ! {self(), {error, Reason}}
        ; (Res, Body) -> ok
            , Waiter ! {self(), {Res, Body}}
        end

    , Options = dot(Options0, ".parent_pid", Waiter)
    , Runner = request(Options, Callback)
    , Result = receive
        {Runner, Received_result} -> ok
            , Received_result
        after 3000 -> ok
            , {error, request_timeout}
        end

    , case Result
        of {error, _Reason} -> ok
            , Result
        ; {Res, Body} -> ok
            , case dot(Options, {".json", false}) =/= false
                of false -> ok
                    , Result
                ; _ -> ok
                    % Decode the JSON.
                    , try ejson:decode(Body)
                        of Obj -> ok
                            , Response = new_response(Res, Obj)
                            , {Response, Response:body()}
                        catch E_type:E_err -> ok
                            , {error, {E_type, E_err}}
                        end
                end
        end
    ;

request(Url, Callback) when is_list(Url) -> ok
    , request({[{url,Url}]}, Callback)
    ;

request({_Opts}=Options, Callback) when is_list(_Opts) andalso is_function(Callback) -> ok
    , Pid = spawn(fun() -> request(child, Options, Callback) end)
    , Pid
    .

request(child, Options, Callback) -> ok
    , Uri = dot(Options, uri)
    , Url = dot(Options, {url, Uri})
    , Body_val = dot(Options, {".body", <<"">>})
    , {Is_json, Body} = case dot(Options, {".json", false})
        of false -> ok
            , {false, Body_val}
        ; true -> ok
            % Encode the .body value.
            , case Body_val
                of <<"">> -> ok
                    , {true, <<"">>}
                ;  _  -> ok
                    , Encoded_body = ejson:encode(Body_val)
                    , {true, Encoded_body}
                end
        ; Json_val -> ok
            % Encode the .json value.
            , Encoded_body = ejson:encode(Json_val)
            , {true, Encoded_body}
        end

    , case Url
        of undefined -> ok
            , throw({bad_arg, "Missing url or uri option"})
        ; _ -> ok
            , Method = dot(Options, {method, get})

            , {ok, Parsed_url} = http_uri:parse(Url)
            , {Proto, _Creds, Hostname, Port, _Path, _Query} = Parsed_url
            , Host = case {Proto, Port}
                of {http, 80} -> Hostname
                ; {https, 443} -> Hostname
                ; _            -> Hostname ++ ":" ++ integer_to_list(Port)
                end

            , Headers = lists:flatten(
                [ {"Host",Host}
                , {"Connection","close"}
                , case Is_json
                    of true -> [{"Accept","application/json"}]
                    ; false -> []
                    end
                , case Method =:= put orelse Method =:= post
                    of false -> []
                    ; true -> ok
                        , Content_type = case Is_json
                            of true -> [{"Content-Type","application/json"}]
                            ; false -> []
                            end
                        , Content_length = case Body
                            of undefined -> []
                            ;  <<"">>    -> []
                            ;  _         -> [{"Content-Length", integer_to_list(size(Body))}]
                            end
                        , [Content_type, Content_length]
                    end
                ])

            , HTTPOptions = []
            , try execute(HTTPOptions, Method, Parsed_url, Headers, Body)
                of {error, Error} -> ok
                    , Callback(error, Error)
                ; {Res, Get_body} -> ok
                    , case dot(Options, ".onResponse")
                        of Response_callback when is_function(Response_callback) -> ok
                            , chown(Res:socket(), dot(Options, {".parent_pid", self()}))
                            , Response_callback(Res, Get_body)
                        ; true -> ok
                            , chown(Res:socket(), dot(Options, {".parent_pid", self()}))
                            , Callback(Res, Get_body)
                        ; _ -> ok
                            , Wrapped = response_callback(Callback)
                            , Wrapped(Res, Get_body)
                        end
                catch A:B -> ok
                    , Callback(error, {A,B})
                end
        end
    .

execute(_HTTPOptions, Method, Uri, Headers, Body) -> ok
    , {_Protocol, _Creds, Host, Port, Path, Query} = Uri
    , Method_ok = list_to_binary(string:to_upper(to_list(Method)))
    , Headers_ok = [ [Key, ": ", Val, "\r\n"] || {Key, Val} <- Headers ]

    , case gen_tcp:connect(Host, Port, [binary, {packet,0}, {active,false}])
        of {error, Error} -> ok
            %, Callback(error, Error)
            , {error, Error}
        ; {ok, Sock} -> ok
            , ok = gen_tcp:send(Sock,
                [ Method_ok, " ", Path, Query, " ", "HTTP/1.1"
                , "\r\n"
                , Headers_ok
                , "\r\n"
                , Body
                ])
            , inet:setopts(Sock, [binary, {packet,http}, {active,false}])
            , recv_headers(Sock, [])
        end
    .

recv_headers(Sock, Headers0) -> ok
    , inet:setopts(Sock, [{active,once}])
    , receive
        {_Type, Sock, {http_response, Version, Code, Reason}} -> ok
            , Headers = [ {http_response, Version, Code, Reason} | Headers0 ]
            , recv_headers(Sock, Headers)
        ; {_Type, Sock, {http_header, _Length, Key, undefined, Value}} -> ok
            , Headers = [ {header, {Key, Value}} | Headers0 ]
            , recv_headers(Sock, Headers)
        ; {_Type, Sock, http_eoh} -> ok
            , Packets = lists:reverse(Headers0)
            , case lists:keytake(http_response, 1, Packets)
                of false -> ok
                    , {error, no_http_response}
                ; {value, RequestPacket, HeaderPackets} -> ok
                    , {http_response, Version, Code, Reason} = RequestPacket
                    , Response_headers = [ {to_atom(Key), Val} || {header, {Key, Val}} <- HeaderPackets ]

                    % Done. Return the response and body receiver.
                    , inet:setopts(Sock, [binary, {packet,0}, {active,false}])
                    , Response = new_response(Sock, {Version, Code, Reason}, Response_headers, undefined)
                    , Body_handler = case Response:headers("transfer-encoding")
                        of "chunked" -> fun() -> get_chunk(Response) end
                        ;  _         -> fun() -> get_body(Response) end
                        end
                    , {Response, Body_handler}
                end
        ; Msg -> ok
            , io:format("Unknown message: ~p\n", [Msg])
            , {error, {http_unknown_message, Msg}}
        after 3000 -> ok
            , {error, header_timeout}
        end
    .

get_body(Response) -> ok
    , Sock = Response:socket()
    , inet:setopts(Sock, [{active,once}])
    , receive
        {_Type, Sock, Data} -> ok
            , Data
        ; {tcp_closed, Sock} -> ok
            , 'end'
        ; Msg -> ok
            , io:format("Unknown message: ~p\n", [Msg])
            , {error, {unknown_message, Msg}}
        after 1000 -> ok
            , {error, data_timeout}
        end
    .

get_chunk(Response) -> ok
    , Body = case erlang:get(pending_data)
        of undefined -> get_body(Response)
        ;  Pending   -> Pending
        end

    , case Body
        of {error, Error} -> ok
            , {error, Error}
        ; Not_bin when not is_binary(Not_bin) -> ok
            , {error, {bad_chunk, Not_bin}}
        ; Chunk -> ok
            , case break_chunk(Chunk)
                of {0, <<"">>} -> ok
                    , 'end'
                ; {Length, Data} when is_integer(Length) andalso is_binary(Data) -> ok
                    , Data
                ; _ -> ok
                    , {error, {bad_chunk, Chunk}}
                end
        end
    .

break_chunk(Chunk) when is_binary(Chunk) -> ok
    , case re:run(Chunk, "^([0-9A-Fa-f]+)\r\n")
        of nomatch -> ok
            , nomatch
        ; {match, [{0, Start_at}, Size_slice]} -> ok
            , Len_b = binary:part(Chunk, Size_slice)
            , Len = list_to_integer(binary_to_list(Len_b), 16)
            , Data = binary:part(Chunk, {Start_at, Len})

            , End_pos = Start_at + Len + 2 % Two characters for \r\n
            , case binary:part(Chunk, {End_pos, size(Chunk) - End_pos})
                of <<"">> -> erlang:erase(pending_data)
                ; Remaining -> erlang:put(pending_data, Remaining)
                end

            , {Len, Data}
        end
    .

% Convert a "full" callback (expecting response, body) into an onResponse callback.
response_callback(Callback) -> ok
    , fun (error, Reason) -> ok
            , Callback(error, Reason)
        ; (Res, Get_data) -> ok
            , {Type, Value} = collect_response(Res, Get_data)
            , Callback(Type, Value)
        end
    .

% Collect all callbacks and return a single response.
collect_response(error, Reason) -> ok
    , {error, Reason}
    ;

collect_response(Res, Get_data) -> ok
    , collect_response(Res, Get_data, <<"">>)
    .

collect_response(Res, Get_data, Current_data) -> ok
    , case Get_data()
        of Data when is_binary(Data) -> ok
            , New_data = <<Current_data/binary, Data/binary>>
            , collect_response(Res, Get_data, New_data)
        ; 'end' -> ok
            , Response = new_response(Res, Current_data)
            , {Response, Response:body()}
        ; Else -> ok
            , Else
        end
    .

get(Url) when is_list(Url) -> ok
    , ?MODULE:get({[{url,Url}]})
    ;

get({Options}) -> ok
    , request(dot({Options}, method, 'get'))
    .

put(Url) when is_list(Url) -> ok
    , ?MODULE:put({[{url, Url}]})
    ;

put({Options}) -> ok
    , request(dot({Options}, method, 'put'))
    .

post(Url) when is_list(Url) -> ok
    , ?MODULE:post({[{url, Url}]})
    ;

post({Options}) -> ok
    , request(dot({Options}, method, 'post'))
    .

delete(Url) when is_list(Url) -> ok
    , ?MODULE:delete({[{url, Url}]})
    ;

delete({Options}) -> ok
    , request(dot({Options}, method, 'delete'))
    .

get(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:get({[{url, Url}]}, Callback)
    ;

get({Options}, Callback) -> ok
    , request(dot({Options}, method, 'get'), Callback)
    .

put(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:put({[{url, Url}]}, Callback)
    ;

put({Options}, Callback) -> ok
    , request(dot({Options}, method, 'put'), Callback)
    .

post(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:post({[{url, Url}]}, Callback)
    ;

post({Options}, Callback) -> ok
    , request(dot({Options}, method, 'post'), Callback)
    .

delete(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:delete({[{url, Url}]}, Callback)
    ;

delete({Options}, Callback) -> ok
    , request(dot({Options}, method, 'delete'), Callback)
    .


%
% Utilities
%

chown(Socket, Pid) -> ok
    %, io:format("chown ~p from ~p to ~p)\n", [Socket, self(), Pid])
    , gen_tcp:controlling_process(Socket, Pid)
    .

dot(Obj, Key) -> request_dot:dot(Obj, Key).
dot(Obj, Key, Val) -> request_dot:dot(Obj, Key, Val).

normal_headers(Headers) -> ok
    , Normalized = normal_headers(Headers, [])
    , {Normalized}
    .

normal_headers([], Normalized) -> ok
    , Normalized
    ;

normal_headers([{Key, Val} | Rest], Normalized) -> ok
    , Normal_key = normal_key(Key)
    , NewNormal = lists:keystore(Normal_key, 1, Normalized, {Normal_key, to_list(Val)})
    , normal_headers(Rest, NewNormal)
    .

normal_key(Key) -> ok
    , string:to_lower(to_list(Key))
    .

to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_number(X) -> integer_to_list(X);
to_list(X) when is_atom(X)   -> atom_to_list(X);
to_list(X) when is_list(X)   -> X.

to_atom(X) when is_list(X) -> erlang:list_to_atom(X);
to_atom(X) when is_binary(X) -> to_atom(binary_to_list(X));
to_atom(X) when is_atom(X) -> X.

% vim: sts=4 sw=4 et
