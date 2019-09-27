% (c) Copyright 2019 Mercer Road Corp. All rights reserved.
% Mercer Road Corp. hereby grants you ("User"), under its copyright
% rights, the right to distribute, reproduce and/or prepare derivative
% works of this software source (or binary) code ("Software") solely
% to the extent expressly authorized in writing by Mercer Road Corp.
% If you do not have a written license from Mercer Road Corp., you
% have no right to use the Software except for internal testing and
% review.
%
% No other rights are granted and no other use is authorized. The
% availability of the Software does not provide any license by
% implication, estoppel, or otherwise under any patent rights or other
% rights owned or controlled by Mercer Road or others covering any use
% of the Software herein.
%
% USER AGREES THAT MERCER ROAD ASSUMES NO LIABILITY FOR ANY DAMAGES,
% WHETHER DIRECT OR OTHERWISE, WHICH THE USER MAY SUFFER DUE TO USER'S
% USE OF THE SOFTWARE.  MERCER ROAD PROVIDES THE SOFTWARE "AS IS,"
% MAKES NO EXPRESS OR IMPLIED REPRESENTATIONS OR WARRANTIES OF ANY
% TYPE, AND EXPRESSLY DISCLAIMS THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT.  USER
% ACKNOWLEDGES THAT IT ASSUMES TOTAL RESPONSIBILITY AND RISK FOR
% USER'S USE OF THE SOFTWARE.
%
% Except for any written authorization, license or agreement from or
% with Mercer Road Corp.  the foregoing terms and conditions shall
% constitute the entire agreement between User and Mercer Road Corp.
% with respect to the subject matter hereof and shall not be modified
% or superceded without the express written authorization of Mercer
% Road Corp.
%
% Any copies or derivative works must include this and all other
% proprietary notices contained on or in the Software as received by
% the User.
%%

-module(mnesia_json).

-export([info/0, json/0]).

-spec(json() -> jsx:json_text()).
json() ->
    Json = jsx:encode(info()),
    jsx:prettify(Json).


-spec(info() -> jsx:json_term()).
info() ->
    Running = mnesia:system_info(running_db_nodes),
    AllNodes = mnesia_lib:all_nodes(),
    Tabs = mnesia:system_info(tables),
    {Unknown, Ram, Disc, DiscOnly, _Ext} =
        lists:foldl(fun mnesia_json_util:storage_count/2, {[], [], [], [], []}, Tabs),
    LiteralTerms = [{FieldName, mnesia:system_info(FieldName)} ||
                 FieldName <-
                           [
                            debug,
                            directory,
                            fallback_activated,
                            schema_location,
                            tables,
                            transaction_commits,
                            transaction_failures,
                            transaction_log_writes,
                            transaction_restarts,
                            use_dir,
                            version
                           ]],
    DerivedTerms = [
             {uncertain_transactions, mnesia_json_util:open_transactions()},
             {running_nodes, Running},
             {db_nodes, AllNodes},
             {stopped_db_nodes, [AllNodes -- Running]},
             {held_locks, length(mnesia:system_info(held_locks))},
             {lock_queue, length(mnesia:system_info(lock_queue))},
             {master_tables, mnesia_recover:get_master_node_tables()},
             {remote, Unknown},
             {ram_copies, Ram},
             {disc, Disc},
             {disc_only, DiscOnly}
            ],
    Term = convert_to_json_term(LiteralTerms ++ DerivedTerms),
    Term ++ [{table_info,
              [{Table, table_info(Table)} || Table <- Tabs]}
            ].


-spec(table_info_fields() -> [atom()]).
table_info_fields() ->
    [
     access_mode,
     active_replicas,
     all_nodes,
     arity,
     attributes,
     disc_copies,
     disc_only_copies,
     index,
     load_node,
     memory,
     ram_copies,
     record_name,
     size,
     storage_type,
     type
    ].

-spec(table_info(atom()) -> jsx:json_term()).
table_info(Table) ->
    TableInfo = [{Field, mnesia:table_info(Table, Field)} ||
                    Field <- table_info_fields()],
    convert_to_json_term(TableInfo).


%% Convert what mnesia:system_info(all) gives us to something that jsx
%% can turn into JSON, a lot of the values that mnesia:system_info/1
%% returns are not convertible to JSON directly. For example it can
%% return a list of pids, or a tuple of the form {1,0} which
%% represents a version number. So explicitly turn those into
%% something that jsx can handle.

convert_to_json_term(TermList) ->
    [{Key, convert_to_jsx_term(Value)} || {Key, Value} <- TermList].


convert_to_jsx_term([]) ->
    [];
convert_to_jsx_term(true) ->
    true;
convert_to_jsx_term(false) ->
    false;
convert_to_jsx_term(Number) when is_number(Number) ->
    Number;
convert_to_jsx_term(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
convert_to_jsx_term({I, J}) when is_number(I) andalso is_number(J) ->
    list_to_binary(io_lib:format("~p.~p", [I, J]));
convert_to_jsx_term({I, J}) when is_atom(I) andalso is_atom(J) ->
    list_to_binary(io_lib:format("~p:~p()", [I, J]));
convert_to_jsx_term(List = [Atom | _]) when is_atom(Atom) ->
    [atom_to_binary(Atom1, utf8) || Atom1 <- List];
convert_to_jsx_term(List = [Pid | _]) when is_pid(Pid) ->
    length(List);
convert_to_jsx_term(String) ->
    list_to_binary(String).
