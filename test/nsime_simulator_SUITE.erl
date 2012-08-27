%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Test module for nsime_simulator
%% Author : Saravanan Vijayakumaran

-module(nsime_simulator_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_simulator_state.hrl").

all() -> [
            test_start_stop
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_start_stop(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),
    ?assertEqual(nsime_simulator:stop(), killed),
    ?assertNot(lists:member(nsime_simulator, erlang:registered())),

    ?assertError(unsupported_scheduler, nsime_simulator:start(junk)),

    nsime_simulator:start(gb_trees),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),
    ?assertEqual(nsime_simulator:stop(), killed),
    ?assertNot(lists:member(nsime_simulator, erlang:registered())),
    ?assertNot(lists:member(nsime_gbtrees_scheduler, erlang:registered())).
