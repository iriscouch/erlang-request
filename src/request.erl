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
            , case httpc:request(Url)
                of {error, Reason} -> ok
                    , {error, Reason}
                ; {ok, Result} -> ok
                    , {Result, <<"I am the body">>}
                end
        end
    .

get(Url) when is_list(Url) -> ok
    , ?MODULE:get({[{url,Url}]})
    ;

get({Options}) -> ok
    , req(oset({Options}, method, 'GET'))
    .

put(Url) when is_list(Url) -> ok
    , ?MODULE:put({[{url, Url}]})
    ;

put({Options}) -> ok
    , req(oset({Options}, method, 'PUT'))
    .

post(Url) when is_list(Url) -> ok
    , ?MODULE:post({[{url, Url}]})
    ;

post({Options}) -> ok
    , req(oset({Options}, method, 'POST'))
    .

delete(Url) when is_list(Url) -> ok
    , ?MODULE:delete({[{url, Url}]})
    ;

delete({Options}) -> ok
    , req(oset({Options}, method, 'DELETE'))
    .

get(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:get({[{url, Url}]}, Callback)
    ;

get({Options}, Callback) -> ok
    , req(oset({Options}, method, 'GET'), Callback)
    .

put(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:put({[{url, Url}]}, Callback)
    ;

put({Options}, Callback) -> ok
    , req(oset({Options}, method, 'PUT'), Callback)
    .

post(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:post({[{url, Url}]}, Callback)
    ;

post({Options}, Callback) -> ok
    , req(oset({Options}, method, 'POST'), Callback)
    .

delete(Url, Callback) when is_list(Url) -> ok
    , ?MODULE:delete({[{url, Url}]}, Callback)
    ;

delete({Options}, Callback) -> ok
    , req(oset({Options}, method, 'DELETE'), Callback)
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

% vim: sts=4 sw=4 et
