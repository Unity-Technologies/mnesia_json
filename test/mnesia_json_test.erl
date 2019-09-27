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

-module(mnesia_json_test).
-include_lib("eunit/include/eunit.hrl").

%% Test records
-record(person,
        {name,
         age = 0,
         address = unknown,
         salary = 0,
         children = []}).


-record(shopping_mall,
        {name,
         address = unknown}).


-spec(jsx_test() -> ok).
jsx_test() ->
    ok = seed_data(),
    Info = mnesia_json:info(),
    ?assert(jsx:is_term(Info)),
    ?assertNotEqual(proplists:get_value(table_info, Info), []),
    application:stop(mnesia),
    ok.


-spec(json_test() -> ok).
json_test() ->
    ok = seed_data(),
    Json = mnesia_json:json(),
    %% Uncomment to show the JSON
    %%?debugFmt("JSON ~n~s~n", [Json]),
    ?assert(jsx:is_json(Json)),
    application:stop(mnesia),
    ok.


-spec(seed_data() -> ok).
seed_data() ->
    mnesia:create_schema([node()]),
    {ok, _} = application:ensure_all_started(mnesia),
    mnesia:delete_table(person),
    mnesia:delete_table(shopping_mall),
    timer:sleep(100),
    mnesia:create_table(person,
                        [{attributes, record_info(fields, person)},
                         {ram_copies, [node()]}
                        ]),
    mnesia:create_table(shopping_mall,
                        [{attributes, record_info(fields, shopping_mall)},
                         {disc_copies, [node()]}
                        ]),
    mnesia:wait_for_tables([person, shopping_mall], 600),
    {atomic, ok} = mnesia:transaction(
                     fun() ->
                             mnesia:write(#person{name = "TEST 1"}),
                             mnesia:write(#person{name = "TEST 2"}),
                             mnesia:write(#person{name = "TEST 3"}),
                             mnesia:write(#person{name = "TEST 4"}),
                             mnesia:write(#person{name = "TEST 5"}),
                             mnesia:write(#shopping_mall{name = "natick mall"}),
                             ok
                     end),
    ok.
