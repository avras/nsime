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
-include("nsime_event.hrl").
-include("nsime_simulator_state.hrl").

all() -> [
            test_start_stop,
            test_schedule_run,
            test_cancel_event
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_start_stop(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),
    ?assertEqual(nsime_simulator:current_time(), {0, sec}),
    ?assertEqual(nsime_simulator:stop(), killed),
    ?assertNot(lists:member(nsime_simulator, erlang:registered())),
    ?assertNot(lists:member(nsime_gbtrees_scheduler, erlang:registered())),

    ?assertError(unsupported_scheduler, nsime_simulator:start(junk)),

    nsime_simulator:start(gb_trees),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),
    ?assertEqual(nsime_simulator:stop(), killed),
    ?assertNot(lists:member(nsime_simulator, erlang:registered())),
    ?assertNot(lists:member(nsime_gbtrees_scheduler, erlang:registered())).

test_schedule_run(_) ->
    nsime_simulator:start(gb_trees),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),

    Time1 = {1, sec},
    Ref1 = make_ref(),
    Event1 = create_event(event1, Ref1, Time1),
    Time2 = {3, sec},
    Ref2 = make_ref(),
    Event2 = create_event(event2, Ref2, Time2),
    Time3 = {6, sec},
    Ref3 = make_ref(),
    Event3 = create_event(event3, Ref3, Time3),
    ?assertEqual(nsime_simulator:schedule(Time2, Event2), Time2),
    ?assertEqual(nsime_simulator:schedule(Time3, Event3), Time3),
    ?assertEqual(nsime_simulator:schedule(Time1, Event1), Time1),
    ?assertEqual(nsime_simulator:run(), simulation_complete),
    receive
        {event1, Ref1} ->
            ok
    end,
    receive
        {event2, Ref2} ->
            ok
    end,
    receive
        {event3, Ref3} ->
            ok
    end,
    ?assertEqual(nsime_simulator:run(), simulation_complete),

    ?assertError(invalid_argument, nsime_simulator:schedule(junk, Event1)),

    ?assertEqual(nsime_simulator:stop(), killed),
    ?assertNot(lists:member(nsime_simulator, erlang:registered())),
    ?assertNot(lists:member(nsime_gbtrees_scheduler, erlang:registered())).

test_cancel_event(_) ->
    nsime_simulator:start(gb_trees),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),

    Time1 = {1, sec},
    Ref1 = make_ref(),
    Event1 = create_event(event1, Ref1, Time1),
    Time2 = {3, sec},
    Ref2 = make_ref(),
    Event2 = create_event(event2, Ref2, Time2),
    Time3 = {6, sec},
    Ref3 = make_ref(),
    Event3 = create_event(event3, Ref3, Time3),
    ?assertEqual(nsime_simulator:schedule(Time2, Event2), Time2),
    ?assertEqual(nsime_simulator:schedule(Time3, Event3), Time3),
    ?assertEqual(nsime_simulator:schedule(Time1, Event1), Time1),
    ?assertEqual(nsime_simulator:cancel(Event2#nsime_event{time = Time2}), ok),
    ?assertEqual(nsime_simulator:run(), simulation_complete),
    receive
        {event1, Ref1} ->
            ok
    end,
    receive
        {event3, Ref3} ->
            ok
    end,

    ?assertEqual(nsime_simulator:cancel(Event2#nsime_event{time = Time2}), none),

    ?assertEqual(nsime_simulator:stop(), killed),
    ?assertNot(lists:member(nsime_simulator, erlang:registered())),
    ?assertNot(lists:member(nsime_gbtrees_scheduler, erlang:registered())).

create_event(Msg, Ref, Time) ->
   #nsime_event{
        eventid = make_ref(),
        module = erlang,
        function = apply,
        arguments = [fun() ->
                        ?assertEqual(nsime_simulator:current_time(), Time),
                        self() ! {Msg, Ref}
                     end, []]
    }.
