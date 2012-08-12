%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Test module for nsime_gbtrees_scheduler
%% Author : Saravanan Vijayakumaran

-module(nsime_gbtrees_scheduler_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [test_gbtrees_scheduler_creation_shutdown].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_gbtrees_scheduler_creation_shutdown(_Config) ->
    ct:log("Creating nsime_gbtrees_scheduler process"),
    nsime_gbtrees_scheduler:create(),
    Pid = erlang:whereis(nsime_gbtrees_scheduler),
        case Pid of
            undefined ->
                ct:fail("Failed to create nsime_gbtrees_scheduler process",[]);
            _ ->
                ct:log("Found nsime_gbtrees_scheduler process in registered process list"),
                ?assert(erlang:is_pid(Pid)),
                ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),
                ct:log("Killing nsime_gbtrees_scheduler process"),
                ?assert(nsime_gbtrees_scheduler:stop()),
                ct:log("Checking nsime_gbtrees_scheduler process has stopped"),
                ?assertNot(lists:member(nsime_gbtrees_scheduler, erlang:registered()))
        end,
    ok.
    
