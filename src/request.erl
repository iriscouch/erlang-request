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
-export([start/0, start/1, api/0, api/1]).

-export([req/1, get/1, put/1, post/1, delete/1]).
-export([req/2, get/2, put/2, post/2, delete/2]).

-export([new_response/1]).
-export([dot/2, dot/3, normal_key/1]).

start() -> ok
    , ssl:start()
    , inets:start()
    .

start(Type) -> ok
    , ssl:start(Type)
    , inets:start(Type)
    .

api() -> ok
    , start()
    , fun ?MODULE:req/1
    .

api(async) -> ok
    , start()
    , fun ?MODULE:req/2
    .

new_response(Headers) when is_list(Headers) -> ok
    % Just fake the reply line since it is unknown.
    , {Version, Status, Reason} = {"1.1", 200, "OK"}
    , new_response({{Version, Status, Reason}, Headers, undefined})
    ;

new_response({{Version_str, Status, Message}, Headers, Body}) -> ok
    , Version = case Version_str
        of "HTTP/" ++ Rest -> Rest
        ;  _               -> to_list(Version_str)
        end
    , request_response:new(Version, Status, Message, normal_headers(Headers), Body)
    .

new_response(Response, New_data) -> ok
    , {Version, Status, Reason} = {Response:httpVersion(), Response:statusCode(), Response:message()}
    , {Headers} = Response:headers()
    , new_response({{Version, Status, Reason}, Headers, New_data})
    .

req(Url) when is_list(Url) -> ok
    , req({[ {url,Url} ]})
    ;

req(Options) -> ok
    , Waiter = self()
    , Callback = fun
        (error, Reason) -> ok
            , Waiter ! {self(), {error, Reason}}
        ; (Res, Body) -> ok
            , Waiter ! {self(), {Res, Body}}
        end

    , Runner = req(Options, Callback)
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
    .

req(Url, Callback) when is_list(Url) -> ok
    , req({[{url,Url}]}, Callback)
    ;

req({_Opts}=Options, Callback) when is_list(_Opts) andalso is_function(Callback) -> ok
    , Pid = spawn(fun() -> req(child, Options, Callback) end)
    , Pid
    .

req(child, Options, Callback) -> ok
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
            , Method = list_to_existing_atom(normal_key(dot(Options, {method, get})))
            , Headers = case Is_json
                of true -> [{"accept","application/json"}]
                ; false -> []
                end
            , Req = case Method
                of _ when Method =/= put andalso Method =/= post -> ok
                    , {Url, Headers}
                ; _ -> ok
                    , Content_type = case Is_json
                        of true -> "application/json"
                        ; false -> ""
                        end
                    , Content_length = case Body
                        of undefined -> "0"
                        ; _          -> integer_to_list(size(Body))
                        end
                    , Headers1 = Headers ++ [{"content-length", Content_length}]
                    , {Url, Headers1, Content_type, Body}
                end

            , Response_callback = case dot(Options, ".onResponse")
                of true -> Callback
                ;  _    -> response_callback(Callback)
                end

            , HTTPOptions = []
            , Req_options = [{sync,false}, {stream,self}, {full_result,true} ]
            , execute([Method, Req, HTTPOptions, Req_options], Response_callback)
        end
    .

execute(Params, Callback) -> ok
    , try apply(httpc, request, Params)
        of {error, Reason} -> ok
            , Callback(error, Reason)
        ; {ok, Req_id} -> ok
            , head(Req_id, Callback)
        catch A:B -> ok
            , Callback(error, {A, B})
        end
    .

head(Req, Callback) -> ok
    , receive
        {http, {Req, stream_start, Headers}} -> ok
            , io:format("HEAD ~p\n", [Headers])
            , Response = new_response(Headers)
            , Body_handler = fun() -> get_body(Req) end
            , Callback(Response, Body_handler)
        ;E -> ok
            , io:format("Else ~p\n", [E])
        after 1000 -> ok
            , {error, headers_timeout}
        end
    .

get_body(Req) -> ok
    , receive
        {http, {Req, stream, Data}} when is_binary(Data) -> ok
            , Data
        ; {http, {Req, stream_end, _Headers}} -> ok
            , 'end'
        after 1000 -> ok
            , {error, data_timeout}
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
        end
    .

get(Url) when is_list(Url) -> ok
    , ?MODULE:get({[{url,Url}]})
    ;

get({Options}) -> ok
    , req(dot({Options}, method, 'get'))
    .

put(Url) when is_list(Url) -> ok
    , ?MODULE:put({[{url, Url}]})
    ;

put({Options}) -> ok
    , req(dot({Options}, method, 'put'))
    .

post(Url) when is_list(Url) -> ok
    , ?MODULE:post({[{url, Url}]})
    ;

post({Options}) -> ok
    , req(dot({Options}, method, 'post'))
    .

delete(Url) when is_list(Url) -> ok
    , ?MODULE:delete({[{url, Url}]})
    ;

delete({Options}) -> ok
    , req(dot({Options}, method, 'delete'))
    .

get(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:get({[{url, Url}]}, Callback)
    ;

get({Options}, Callback) -> ok
    , req(dot({Options}, method, 'get'), Callback)
    .

put(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:put({[{url, Url}]}, Callback)
    ;

put({Options}, Callback) -> ok
    , req(dot({Options}, method, 'put'), Callback)
    .

post(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:post({[{url, Url}]}, Callback)
    ;

post({Options}, Callback) -> ok
    , req(dot({Options}, method, 'post'), Callback)
    .

delete(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:delete({[{url, Url}]}, Callback)
    ;

delete({Options}, Callback) -> ok
    , req(dot({Options}, method, 'delete'), Callback)
    .


%
% Utilities
%

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

% vim: sts=4 sw=4 et
