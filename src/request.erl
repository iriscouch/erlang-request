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
-export([normal_key/1]).

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

req({Options}) when is_list(Options) -> ok
    , Uri = oget(Options, uri)
    , Url = oget(Options, url, Uri)
    , case Url
        of undefined -> ok
            , throw({bad_arg, "Missing url or uri option"})
        ; _ -> ok
            , Method = list_to_existing_atom(normal_key(oget(Options, method, get)))
            , Headers = []
            , HTTPOptions = []
            , Req_options = []
            %, Profile = httpc:default_profile()
            , Req = case Method
                of _ when Method =:= put orelse Method =:= post -> ok
                    , ContentType = ""
                    , Body = ""
                    , {Url, Headers, ContentType, Body}
                ; _ -> ok
                    , {Url, Headers}
                end

            %, io:format("=== httpc:request(~p, ~p, ~p, ~p).\n", [Method, Req, Req_options, HTTPOptions])
            , try httpc:request(Method, Req, Req_options, HTTPOptions)
                of {error, Reason} -> ok
                    , {error, Reason}
                ; {ok, Result} -> ok
                    , {Result, <<"I am the body">>}
                catch A:B -> ok
                    , io:format("httpc error: ~p:~p\n", [A, B])
                    , exit({A, B})
                end
        end
    .


get(Url) when is_list(Url) -> ok
    , ?MODULE:get({[{url,Url}]})
    ;

get({Options}) -> ok
    , req(oset({Options}, method, 'get'))
    .

put(Url) when is_list(Url) -> ok
    , ?MODULE:put({[{url, Url}]})
    ;

put({Options}) -> ok
    , req(oset({Options}, method, 'put'))
    .

post(Url) when is_list(Url) -> ok
    , ?MODULE:post({[{url, Url}]})
    ;

post({Options}) -> ok
    , req(oset({Options}, method, 'post'))
    .

delete(Url) when is_list(Url) -> ok
    , ?MODULE:delete({[{url, Url}]})
    ;

delete({Options}) -> ok
    , req(oset({Options}, method, 'delete'))
    .

get(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:get({[{url, Url}]}, Callback)
    ;

get({Options}, Callback) -> ok
    , req(oset({Options}, method, 'get'), Callback)
    .

put(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:put({[{url, Url}]}, Callback)
    ;

put({Options}, Callback) -> ok
    , req(oset({Options}, method, 'put'), Callback)
    .

post(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:post({[{url, Url}]}, Callback)
    ;

post({Options}, Callback) -> ok
    , req(oset({Options}, method, 'post'), Callback)
    .

delete(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:delete({[{url, Url}]}, Callback)
    ;

delete({Options}, Callback) -> ok
    , req(oset({Options}, method, 'delete'), Callback)
    .


waiter(Options, Callback) -> ok
    , {One, Two} = req({Options})
    , Callback(One, Two)
    .

%
% Utilities
%

as_binary(B) when is_binary(B) ->
    B;
as_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
as_binary(L) when is_list(L) ->
    list_to_binary(L).

odel(Obj, Key) -> ok
    , oset(Obj, Key, undefined)
    .

oset({Obj}, Key, Value) -> ok
    , Result = oset(Obj, Key, Value)
    , {Result}
    ;

oset(Obj, Key, undefined) -> ok
    , lists:keydelete(Key, 1, Obj)
    ;

oset(Obj, Key, Value) -> ok
    , lists:keystore(Key, 1, Obj, {Key, Value})
    .

oget(Obj, Key) -> ok
    , oget(Obj, Key, undefined)
    .

oget({Obj}, Key, Default) -> ok
    , oget(Obj, Key, Default)
    ;

oget(Obj, Key, Default) when is_list(Obj) -> ok
    , case lists:keyfind(Key, 1, Obj)
        of {Key, Value} -> ok
            , Value
        ; false -> ok
            , case is_atom(Key)
                of true -> oget(Obj, as_binary(Key), Default)
                ; false -> Default
                end
        end
    .

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
