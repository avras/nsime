%%
%%  Copyright (C) 2012 Saravanan Vijayakumaran <sarva.v@gmail.com>
%%
%%  This file is part of nsime.
%%
%%  nsime is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  nsime is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with nsime.  If not, see <http://www.gnu.org/licenses/>.
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
            test_cancel_event,
            test_cast_info_codechange
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
    ?assertEqual(nsime_simulator:stop(), stopped),
    ?assertNot(lists:member(nsime_simulator, erlang:registered())),
    ?assertNot(lists:member(nsime_gbtrees_scheduler, erlang:registered())),

    ?assertError(unsupported_scheduler, nsime_simulator:start(junk)),

    nsime_simulator:start(gb_trees),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),
    ?assertEqual(nsime_simulator:stop(), stopped),
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
    ?assertEqual(nsime_simulator:schedule(Time2, Event2), Event2),
    ?assertEqual(nsime_simulator:schedule(Time3, Event3), Event3),
    ?assertEqual(nsime_simulator:schedule(Time1, Event1), Event1),
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

    ?assertEqual(nsime_simulator:stop(), stopped),
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
    ?assertEqual(nsime_simulator:schedule(Time2, Event2), Event2),
    ?assertEqual(nsime_simulator:schedule(Time3, Event3), Event3),
    ?assertEqual(nsime_simulator:schedule(Time1, Event1), Event1),
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

    ?assertEqual(nsime_simulator:stop(), stopped),
    ?assertNot(lists:member(nsime_simulator, erlang:registered())),
    ?assertNot(lists:member(nsime_gbtrees_scheduler, erlang:registered())).

test_cast_info_codechange(_) ->
    nsime_simulator:start(),
    Pid = erlang:whereis(nsime_simulator),
    ?assert(erlang:is_pid(Pid)),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    gen_server:cast(nsime_simulator, junk),
    Pid ! junk,
    nsime_simulator:code_change(junk, junk, junk),
    ?assertEqual(nsime_simulator:stop(), stopped).

create_event(Msg, Ref, Time) ->
   #nsime_event{
        time = Time,
        eventid = make_ref(),
        module = erlang,
        function = apply,
        arguments = [fun() ->
                        ?assertEqual(nsime_simulator:current_time(), Time),
                        self() ! {Msg, Ref}
                     end, []]
    }.
