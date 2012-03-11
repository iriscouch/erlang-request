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

-module(request_response, [Version, Status, Message, Headers, Body]).
-author('Jason Smith <jhs@iriscouch.com>').

-export([httpVersion/0, statusCode/0, message/0, body/0]).
-export([headers/0, headers/1]).

httpVersion() -> Version.

statusCode() -> Status.

message() -> Message.

body() -> Body.

headers() -> Headers.

headers(Key) -> ok
    , {Obj} = Headers
    , Normal_key = request:normal_key(Key)
    , case lists:keyfind(Normal_key, 1, Obj)
        of {Normal_key, Value} -> Value
        ;  _                   -> undefined
        end
    .

% vim: sts=4 sw=4 et
