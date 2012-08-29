%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Test module for nsime_scheduler
%% Author : Saravanan Vijayakumaran

-module(nsime_scheduler_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [
            test_behavior
         ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_behavior(_) ->
    Callbacks = [
        {create, 0},
        {insert, 1},
        {is_empty, 0},
        {remove, 1},
        {remove_next, 0},
        {stop, 0}
    ],
    ?assertEqual(nsime_scheduler:behaviour_info(callbacks), Callbacks),
    ?assertEqual(nsime_scheduler:behaviour_info(junk), undefined).
