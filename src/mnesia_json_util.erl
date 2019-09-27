%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

% NOTICE: This file contains code taken from the Erlang source file
% mnesia.erl

-module(mnesia_json_util).

-export([open_transactions/0, storage_count/2]).

-spec open_transactions() -> [binary()].
open_transactions() ->
    Pat = {'_', unclear, '_'},
    Uncertain = ets:match_object(mnesia_decision, Pat),
    lists:map(
      fun({Tid, _, Nodes}) ->
              list_to_binary(io_lib:format("Tid ~w waits for decision "
                                           "from ~w~n",
                                           [Tid, Nodes]))
      end, Uncertain).


-spec storage_count(atom(), {_, _, _, _, _}) -> {_, _, _, _, _}.
storage_count(T, {U, R, D, DO, Ext}) ->
    case mnesia:table_info(T, storage_type) of
        unknown -> {[T | U], R, D, DO, Ext};
        ram_copies -> {U, [T | R], D, DO, Ext};
        disc_copies -> {U, R, [T | D], DO, Ext};
        disc_only_copies -> {U, R, D, [T | DO], Ext};
        {ext, A, _} -> {U, R, D, DO, orddict:append(A, T, Ext)}
    end.
