%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Test module for nsime_data_rate
%% Author : Saravanan Vijayakumaran

-module(nsime_data_rate_SUITE).
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
          test_is_nsime_data_rate_unit,
          test_is_nsime_data_rate,
          test_calc_tx_time
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

test_is_nsime_data_rate_unit(_) ->
    ?assert(nsime_data_rate:is_nsime_data_rate_unit(bits_per_sec)),
    ?assert(nsime_data_rate:is_nsime_data_rate_unit(bytes_per_sec)),
    ?assert(nsime_data_rate:is_nsime_data_rate_unit(kilo_bits_per_sec)),
    ?assert(nsime_data_rate:is_nsime_data_rate_unit(kilo_bytes_per_sec)),
    ?assert(nsime_data_rate:is_nsime_data_rate_unit(mega_bits_per_sec)),
    ?assert(nsime_data_rate:is_nsime_data_rate_unit(mega_bytes_per_sec)),
    ?assert(nsime_data_rate:is_nsime_data_rate_unit(giga_bits_per_sec)),
    ?assert(nsime_data_rate:is_nsime_data_rate_unit(giga_bytes_per_sec)),
    ?assertNot(nsime_data_rate:is_nsime_data_rate_unit(junk)).

test_is_nsime_data_rate(_) ->
    ?assert(nsime_data_rate:is_nsime_data_rate({0, bits_per_sec})),
    ?assert(nsime_data_rate:is_nsime_data_rate({0.5, bits_per_sec})),
    ?assertNot(nsime_data_rate:is_nsime_data_rate({-1, bits_per_sec})),
    ?assertNot(nsime_data_rate:is_nsime_data_rate({1, junk})),
    ?assertNot(nsime_data_rate:is_nsime_data_rate({-1, junk})),
    ?assertNot(nsime_data_rate:is_nsime_data_rate(junk)).

test_calc_tx_time(_) ->
    ?assertError(invalid_argument, nsime_data_rate:calc_tx_time(junk, 0)),
    ?assertError(invalid_argument, nsime_data_rate:calc_tx_time({-1, bits_per_sec}, 1)),
    ?assertError(invalid_argument, nsime_data_rate:calc_tx_time({1, bits_per_sec}, -1)),
    ?assertError(invalid_argument, nsime_data_rate:calc_tx_time({-1, bits_per_sec}, 1)),
    ?assertError(invalid_argument, nsime_data_rate:calc_tx_time({1, junk}, 1)),
    ?assertEqual(nsime_data_rate:calc_tx_time({1, bits_per_sec}, 1), {8.0, sec}),
    ?assertEqual(nsime_data_rate:calc_tx_time({1, bytes_per_sec}, 1), {1.0, sec}),
    ?assertEqual(nsime_data_rate:calc_tx_time({1, kilo_bits_per_sec}, 1000), {8.0, sec}),
    ?assertEqual(nsime_data_rate:calc_tx_time({1, kilo_bytes_per_sec}, 1000), {1.0, sec}),
    ?assertEqual(nsime_data_rate:calc_tx_time({1, mega_bits_per_sec}, 1000000), {8.0, sec}),
    ?assertEqual(nsime_data_rate:calc_tx_time({1, mega_bytes_per_sec}, 1000000), {1.0, sec}),
    ?assertEqual(nsime_data_rate:calc_tx_time({1, giga_bits_per_sec}, 1000000000), {8.0, sec}),
    ?assertEqual(nsime_data_rate:calc_tx_time({1, giga_bytes_per_sec}, 1000000000), {1.0, sec}).
