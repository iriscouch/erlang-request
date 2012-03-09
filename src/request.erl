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
-export([api/0]).
-export([req/1, get/1, put/1, post/1, delete/1]).
%-export([req/2, get/2, put/2, post/2, delete/2]).

api() -> fun req/1.

%req(Uri, Callback) when is_list(Uri) andalso is_function(Callback) -> ok
req(Url) -> ok
    .

get(Uri) -> ok
    .

put(Uri) -> ok
    .

post(Uri) -> ok
    .

delete(Uri) -> ok
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

% vim: sts=4 sw=4 et
