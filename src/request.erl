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

new_response({{Version_str, Status, Message}, Headers, Body}) -> ok
    , Version = case Version_str
        of "HTTP/" ++ Rest -> Rest
        ; _                -> undefined
        end
    , request_response:new(Version, Status, Message, normal_headers(Headers), Body)
    .

req(Url, Callback) when is_list(Url) -> ok
    , req({[{url,Url}]}, Callback)
    ;

req({Options}, Callback) when is_list(Options) andalso is_function(Callback) -> ok
    , Request_pid = spawn(fun() -> waiter(Options, Callback) end)
    , Request_pid
    .

req(Url) when is_list(Url) -> ok
    , req({[{url, Url}]})
    ;

req({Obj}=Options) when is_list(Obj) -> ok
    , Uri = dot(Options, uri)
    , Url = dot(Options, {url, Uri})
    , Is_json = dot(Options, {".json", false}) =/= false

    , case Url
        of undefined -> ok
            , throw({bad_arg, "Missing url or uri option"})
        ; _ -> ok
            , Method = list_to_existing_atom(normal_key(dot(Options, {method, get})))
            , Headers = case Is_json
                of true -> [{"accept","application/json"}]
                ; false -> []
                end
            , HTTPOptions = []
            , Req_options = []
            %, Profile = httpc:default_profile()
            , Req = case Method
                of put -> {Url, Headers, "application/json", ""}
                ; post -> {Url, Headers, "application/json", ""}
                ; _    -> {Url, Headers}
                end

            %, io:format("httpc:request(~p, ~p, ~p, ~p).\n", [Method, Req, Req_options, HTTPOptions])
            , try httpc:request(Method, Req, Req_options, HTTPOptions)
                of {error, Reason} -> ok
                    , {error, Reason}
                ; {ok, {Res_head, Res_headers, Res_body}=Res} -> ok
                    , case Is_json
                        of false -> ok
                            % No JSON decoding.
                            , Response = new_response(Res)
                            , {Response, Response:body()}
                        ;  _ -> ok
                            % Decode the JSON.
                            , try ejson:decode(Res_body)
                                of Eobj -> ok
                                    , Response = new_response({Res_head, Res_headers, Eobj})
                                    , {Response, Response:body()}
                                catch E_type:E_err -> ok
                                    , {error, {E_type, E_err}}
                                end
                        end
                catch A:B -> ok
                    , {error, {A, B}}
                end
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


waiter(Options, Callback) -> ok
    , {One, Two} = req({Options})
    , Callback(One, Two)
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
