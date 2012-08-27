%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Test module for nsime_time
%% Author : Saravanan Vijayakumaran

-module(nsime_time_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").

all() -> [
          {group, testgroup_all}
         ].

groups() ->
    [{
        testgroup_all,
        [parallel],
        [
          test_is_nsime_time_unit,
          test_is_nsime_time,
          test_add,
          test_value
        ]
    }].
          

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(testgroup_all, Config) ->
    Config.

end_per_group(testgroup_all, Config) ->
    Config.

test_is_nsime_time_unit(_) ->
    ?assert(nsime_time:is_nsime_time_unit(sec)),
    ?assert(nsime_time:is_nsime_time_unit(milli_sec)),
    ?assert(nsime_time:is_nsime_time_unit(micro_sec)),
    ?assert(nsime_time:is_nsime_time_unit(nano_sec)),
    ?assertNot(nsime_time:is_nsime_time_unit(junk)).

test_is_nsime_time(_) ->
    ?assert(nsime_time:is_nsime_time({0, sec})),
    ?assert(nsime_time:is_nsime_time({0.5, sec})),
    ?assertNot(nsime_time:is_nsime_time({-1, sec})),
    ?assertNot(nsime_time:is_nsime_time({1, junk})),
    ?assertNot(nsime_time:is_nsime_time({-1, junk})),
    ?assertNot(nsime_time:is_nsime_time(junk)).

test_add(_) ->
    ?assertError(invalid_argument, nsime_time:add(0,0)),
    ?assertError(invalid_argument, nsime_time:add(0,{1, sec})),
    ?assertError(invalid_argument, nsime_time:add({1, sec}, 0)),
    ?assertError(invalid_argument, nsime_time:add({-1, sec}, {2, sec})),
    ?assertEqual(nsime_time:add({1, sec}, {2, sec}), {3, sec}),
    ?assertEqual(nsime_time:add({1, sec}, {0, sec}), {1, sec}),
    ?assertEqual(nsime_time:add({1, sec}, {1, milli_sec}), {1001, milli_sec}),
    ?assertEqual(nsime_time:add({1, sec}, {1, micro_sec}), {1000001, micro_sec}),
    ?assertEqual(nsime_time:add({1, sec}, {1, nano_sec}), {1000000001, nano_sec}),
    ?assertEqual(nsime_time:add({1, milli_sec}, {1, sec}), {1001, milli_sec}),
    ?assertEqual(nsime_time:add({1, milli_sec}, {1, micro_sec}), {1001, micro_sec}),
    ?assertEqual(nsime_time:add({1, milli_sec}, {1, nano_sec}), {1000001, nano_sec}),
    ?assertEqual(nsime_time:add({1, micro_sec}, {1, sec}), {1000001, micro_sec}),
    ?assertEqual(nsime_time:add({1, micro_sec}, {1, milli_sec}), {1001, micro_sec}),
    ?assertEqual(nsime_time:add({1, micro_sec}, {1, nano_sec}), {1001, nano_sec}),
    ?assertEqual(nsime_time:add({1, nano_sec}, {1, sec}), {1000000001, nano_sec}),
    ?assertEqual(nsime_time:add({1, nano_sec}, {1, milli_sec}), {1000001, nano_sec}),
    ?assertEqual(nsime_time:add({1, nano_sec}, {1, micro_sec}), {1001, nano_sec}).

test_value(_) ->
    ?assertError(invalid_argument, nsime_time:value(junk)),
    ?assertEqual(nsime_time:value({0, sec}), 0),
    ?assertEqual(nsime_time:value({0, milli_sec}), 0),
    ?assertEqual(nsime_time:value({0, micro_sec}), 0),
    ?assertEqual(nsime_time:value({0, nano_sec}), 0),
    ?assert(nsime_time:value({1.5, sec}) == 1500000000),
    ?assert(nsime_time:value({1.5, milli_sec}) == 1500000),
    ?assert(nsime_time:value({1.5, micro_sec}) == 1500),
    ?assert(nsime_time:value({1.5, nano_sec}) == 1.5).
