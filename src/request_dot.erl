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

-module(request_dot).
-export([dot/2, dot/3]).

dot(Obj, {Key, Default}) -> ok
    , case dot(Obj, Key)
        of undefined -> Default
        ;  Defined   -> Defined
        end
    ;

dot(Obj, Key) when is_atom(Key) -> ok
    , dot(Obj, atom_to_list(Key))
    ;

dot({Obj}, Key) when is_list(Obj) andalso is_list(Key) -> ok
    , {_Key, Val} = get_dot({Obj}, string:tokens(Key, "."))
    , Val
    .

dot(Obj, Key, Val) when is_atom(Key) -> ok
    , dot(Obj, atom_to_list(Key), Val)
    ;

dot(Obj, Key, Val) when is_list(Val) -> ok
    , dot(Obj, Key, list_to_binary(Val))
    ;

dot({Obj}, Key, Val) when is_list(Obj) andalso is_list(Key) -> ok
    , set_dot({Obj}, string:tokens(Key, "."), Val)
    .

get_dot(Obj, Keys) -> ok
    , get_dot(Obj, Keys, undefined)
    .

get_dot(Obj, [], Last_key) -> ok
    , {Last_key, Obj}
    ;

get_dot({Obj}, [Key | Rest], _Last_key) -> ok
    , Key_atom = as_atom(Key)
    , {Good_key, Value} = case lists:keyfind(Key_atom, 1, Obj)
        of {Key_atom, Value_a} -> ok
            , {Key_atom, Value_a}
        ; false -> ok
            , Key_bin = as_binary(Key)
            , case lists:keyfind(Key_bin, 1, Obj)
                of {Key_bin, Value_b} -> ok
                    , {Key_bin, Value_b}
                ; false -> ok
                    , Key_str = as_list(Key)
                    , case lists:keyfind(Key_str, 1, Obj)
                        of {Key_str, Value_s} -> {Key_str, Value_s}
                        ;  false              -> {Key, undefined}
                        end
                end
        end
    , get_dot(Value, Rest, Good_key)
    ;

get_dot(Non_obj, [Key | _Rest], _Last_key) -> ok
    , Msg = io_lib:format("Non-Object has no key: ~p", [Key])
    , throw({dot, lists:flatten(Msg), Non_obj})
    .

set_dot({Obj}, [Key], Val) -> ok
    , {Good_key, _Old_val} = get_dot({Obj}, [Key])
    , New_obj = case Val
        of undefined -> ok
            , lists:keydelete(Good_key, 1, Obj)
        ; _ -> ok
            , lists:keystore(Good_key, 1, Obj, {Good_key, Val})
        end
    , {New_obj}
    ;

set_dot({Obj}, [Key | Rest], Val) -> ok
    , {Good_key, Old_inner} = get_dot({Obj}, [Key])
    , New_inner = set_dot(Old_inner, Rest, Val)
    , New_obj = lists:keystore(Good_key, 1, Obj, {Good_key, New_inner})
    , {New_obj}
    .

%
% Utilities
%

as_binary(B) when is_binary(B) -> B;
as_binary(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
as_binary(L) when is_list(L) -> list_to_binary(L).

as_list(B) when is_binary(B) -> binary_to_list(B);
as_list(A) when is_atom(A) -> atom_to_list(A);
as_list(L) when is_list(L) -> L.

as_atom(A) when is_atom(A) -> A;
as_atom(B) when is_binary(B) -> as_atom(binary_to_list(B));
as_atom(L) when is_list(L) -> ok
    , try list_to_existing_atom(L)
        of Atom -> Atom
        catch error:badarg -> {error, not_atom}
        end
    .

% vim: sts=4 sw=4 et
