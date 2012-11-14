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

%% Purpose : Test module for nsime_gbtrees_scheduler
%% Author : Saravanan Vijayakumaran

-module(nsime_gbtrees_scheduler_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_event.hrl").

all() -> [
          test_creation,
          {group, testgroup_insertion_deletion}
         ].

groups() ->
    [{
        testgroup_insertion_deletion,
        [sequence],
        [
          test_empty_initally,
          test_insert_single_event,
          test_remove_next_single_event,
          test_remove_single_event,
          test_insert_remove_next_events_unique_timestamps,
          test_insert_remove_next_events_duplicate_timestamps,
          test_insert_remove_events_unique_timestamps,
          test_insert_remove_events_duplicate_timestamps,
          test_remove_next_simultaneous
        ]
    }].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(testgroup_insertion_deletion, Config) ->
    Config.

end_per_group(testgroup_insertion_deletion, Config) ->
    Config.

test_creation(_) ->
    SchedulerState = nsime_gbtrees_scheduler:create(),
    ?assertEqual(SchedulerState, gb_trees:empty()).

test_empty_initally(_) ->
    SchedulerState = nsime_gbtrees_scheduler:create(),
    ?assert(nsime_gbtrees_scheduler:is_empty(SchedulerState)).

test_insert_single_event(_) ->
    SchedulerState = nsime_gbtrees_scheduler:create(),
    ?assert(nsime_gbtrees_scheduler:is_empty(SchedulerState)),
    Time = {5, sec},
    Event = create_nsime_event(Time),
    SchedulerState1 = nsime_gbtrees_scheduler:insert(SchedulerState, Event),
    ?assertNot(nsime_gbtrees_scheduler:is_empty(SchedulerState1)).

test_remove_next_single_event(_) ->
    SchedulerState = nsime_gbtrees_scheduler:create(),
    ?assert(nsime_gbtrees_scheduler:is_empty(SchedulerState)),
    Time = {6, sec},
    Event = create_nsime_event(Time),
    SchedulerState1 = nsime_gbtrees_scheduler:insert(SchedulerState, Event),
    {Event, SchedulerState2} = nsime_gbtrees_scheduler:remove_next(SchedulerState1),
    ?assert(nsime_gbtrees_scheduler:is_empty(SchedulerState2)),
    ?assertEqual(nsime_gbtrees_scheduler:remove_next(SchedulerState2), none).

test_remove_single_event(_) ->
    SchedulerState = nsime_gbtrees_scheduler:create(),
    ?assert(nsime_gbtrees_scheduler:is_empty(SchedulerState)),
    Time = {6, sec},
    Event = create_nsime_event(Time),
    SchedulerState1 = nsime_gbtrees_scheduler:insert(SchedulerState, Event),
    {ok, SchedulerState2} = nsime_gbtrees_scheduler:remove(SchedulerState1, Event),
    ?assert(nsime_gbtrees_scheduler:is_empty(SchedulerState2)),
    ?assertEqual(nsime_gbtrees_scheduler:remove(SchedulerState2, Event), none),
    NewTime = nsime_time:add(Time, {1, sec}),
    SchedulerState3 = nsime_gbtrees_scheduler:insert(SchedulerState2, Event#nsime_event{time = NewTime}),
    ?assertEqual(nsime_gbtrees_scheduler:remove(SchedulerState3, Event), none).

test_insert_remove_next_events_unique_timestamps(_) ->
    N = 100,
    Timestamps = lists:zip(lists:seq(1,N), lists:duplicate(N, sec)),
    insert_remove_next_events_from_timestamps(Timestamps).

test_insert_remove_next_events_duplicate_timestamps(_) ->
    N = 100,
    Time = {73, sec},
    Timestamps = lists:duplicate(N, Time),
    insert_remove_next_events_from_timestamps(Timestamps).

insert_remove_next_events_from_timestamps(Timestamps) ->
    SchedulerState = nsime_gbtrees_scheduler:create(),
    EventList = lists:map(fun(Time) -> create_nsime_event(Time) end, Timestamps),
    SchedulerState1 = lists:foldl(
        fun(Event, State) ->
            nsime_gbtrees_scheduler:insert(State, Event)
        end,
        SchedulerState,
        EventList
    ),
    SchedulerState2 = lists:foldl(
        fun(_, State) ->
             {Event, NewState} = nsime_gbtrees_scheduler:remove_next(State),
             ?assert(lists:member(Event, EventList)),
             NewState
         end,
         SchedulerState1,
         Timestamps
    ),
    ?assert(nsime_gbtrees_scheduler:is_empty(SchedulerState2)),
    ?assertEqual(nsime_gbtrees_scheduler:remove_next(SchedulerState2), none).

test_insert_remove_events_unique_timestamps(_) ->
    N = 100,
    Timestamps = lists:zip(lists:seq(1,N), lists:duplicate(N, sec)),
    insert_remove_events_from_timestamps(Timestamps).

test_insert_remove_events_duplicate_timestamps(_) ->
    N = 100,
    Time = {73, sec},
    Timestamps = lists:duplicate(N, Time),
    insert_remove_events_from_timestamps(Timestamps).


insert_remove_events_from_timestamps(Timestamps) ->
    SchedulerState = nsime_gbtrees_scheduler:create(),
    EventList = lists:map(fun (Time) -> create_nsime_event(Time) end, Timestamps),
    SchedulerState1 = lists:foldl(
        fun(Event, State) ->
            nsime_gbtrees_scheduler:insert(State, Event)
        end,
        SchedulerState,
        EventList
    ),
    SchedulerState2 = lists:foldl(
        fun(Event, State) ->
             {ok, NewState} = nsime_gbtrees_scheduler:remove(State, Event),
             NewState
         end,
         SchedulerState1,
         EventList
    ),
    ?assertEqual(SchedulerState2, gb_trees:empty()).

test_remove_next_simultaneous(_) ->
    N = 100,
    Time = {73, sec},
    Timestamps = lists:duplicate(N, Time),
    SchedulerState = nsime_gbtrees_scheduler:create(),
    ?assertEqual(nsime_gbtrees_scheduler:remove_next_simultaneous(SchedulerState), none),
    EventList = lists:map(fun(T) -> create_nsime_event(T) end, Timestamps),
    SchedulerState1 = lists:foldl(
        fun(Event, State) ->
            nsime_gbtrees_scheduler:insert(State, Event)
        end,
        SchedulerState,
        EventList
    ),
    {NextSimultaneousEvents, SchedulerState2} = nsime_gbtrees_scheduler:remove_next_simultaneous(SchedulerState1),
    ?assertEqual(SchedulerState2, gb_trees:empty()),
    lists:foreach(
        fun(Event) ->
            ?assert(lists:member(Event, EventList))
        end,
        NextSimultaneousEvents
    ).

create_nsime_event(Time) ->    
    #nsime_event{
        time = Time,
        pid = erlang:self(),
        module = erlang,
        function = date,
        eventid = make_ref()
    }.

