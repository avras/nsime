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

-include("nsime_types.hrl").
-include("nsime_event.hrl").

all() -> [
          test_creation_shutdown,
          {group, testgroup_insertion_deletion},
          test_cast_info_codechange
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
          test_insert_remove_events_duplicate_timestamps
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

test_creation_shutdown(_) ->
    nsime_gbtrees_scheduler:create(),
    Pid = erlang:whereis(nsime_gbtrees_scheduler),
        case Pid of
            undefined ->
                ct:fail("Failed to create nsime_gbtrees_scheduler process",[]);
            _ ->
                ?assert(erlang:is_pid(Pid)),
                ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),
                ?assertEqual(nsime_gbtrees_scheduler:stop(), stopped),
                ?assertNot(lists:member(nsime_gbtrees_scheduler, erlang:registered()))
        end.

test_empty_initally(_) ->
    nsime_gbtrees_scheduler:create(),
    ?assert(nsime_gbtrees_scheduler:is_empty()),
    nsime_gbtrees_scheduler:stop().

test_insert_single_event(_) ->
    nsime_gbtrees_scheduler:create(),
    ?assert(nsime_gbtrees_scheduler:is_empty()),
    Time = {5, sec},
    Event = create_nsime_event(Time),
    ?assertEqual(nsime_gbtrees_scheduler:insert(Event), ok),
    ?assertNot(nsime_gbtrees_scheduler:is_empty()),
    EventQueue = nsime_gbtrees_scheduler:get_event_queue(),
    case gb_trees:lookup(nsime_time:value(Time), EventQueue) of
        {value, [FirstEvent | RestOfEvents]} ->
            ?assertEqual(FirstEvent, Event),
            ?assertEqual(RestOfEvents, []);
        _ ->
            ?assert(false)
    end,
    nsime_gbtrees_scheduler:stop().

test_remove_next_single_event(_) ->
    nsime_gbtrees_scheduler:create(),
    Time = {6, sec},
    Event = create_nsime_event(Time),
    nsime_gbtrees_scheduler:insert(Event),
    ?assertEqual(nsime_gbtrees_scheduler:remove_next(), Event),
    ?assert(nsime_gbtrees_scheduler:is_empty()),
    ?assertEqual(nsime_gbtrees_scheduler:remove_next(), none),
    nsime_gbtrees_scheduler:stop().

test_remove_single_event(_) ->
    nsime_gbtrees_scheduler:create(),
    Time = {6, sec},
    Event = create_nsime_event(Time),
    nsime_gbtrees_scheduler:insert(Event),
    ?assertEqual(nsime_gbtrees_scheduler:remove(Event), ok),
    ?assert(nsime_gbtrees_scheduler:is_empty()),
    ?assertEqual(nsime_gbtrees_scheduler:remove(Event), none),
    NewTime = nsime_time:add(Time, {1, sec}),
    nsime_gbtrees_scheduler:insert(Event#nsime_event{time = NewTime}),
    ?assertEqual(nsime_gbtrees_scheduler:remove(Event), none),
    nsime_gbtrees_scheduler:stop().


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
    nsime_gbtrees_scheduler:create(),
    EventList = lists:map(fun (Time) -> create_nsime_event(Time) end, Timestamps),
    ReturnCodes = lists:map(fun (Event) -> nsime_gbtrees_scheduler:insert(Event) end, EventList),
    lists:map(fun (Code) -> ?assertEqual(Code, ok) end, ReturnCodes),
    lists:map(fun (_) ->
                  Event = nsime_gbtrees_scheduler:remove_next(),
                  ?assert(lists:member(Event, EventList))
              end,
              Timestamps
             ),
    ?assert(nsime_gbtrees_scheduler:is_empty()),
    ?assertEqual(nsime_gbtrees_scheduler:remove_next(), none),
    nsime_gbtrees_scheduler:stop().

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
    nsime_gbtrees_scheduler:create(),
    EventList = lists:map(fun (Time) -> create_nsime_event(Time) end, Timestamps),
    InsertReturnCodes = lists:map(fun (Event) -> nsime_gbtrees_scheduler:insert(Event) end, EventList),
    lists:map(fun (Code) -> ?assertEqual(Code, ok) end, InsertReturnCodes),
    RemoveReturnCodes = lists:map(fun (Event) -> nsime_gbtrees_scheduler:remove(Event) end, EventList),
    ?assert(nsime_gbtrees_scheduler:is_empty()),
    lists:map(fun (Code) -> ?assertEqual(Code, ok) end, RemoveReturnCodes),
    Event = create_nsime_event(5),
    ?assertEqual(nsime_gbtrees_scheduler:remove(Event), none),
    nsime_gbtrees_scheduler:stop().

test_cast_info_codechange(_) ->
    nsime_gbtrees_scheduler:create(),
    Pid = erlang:whereis(nsime_gbtrees_scheduler),
    ?assert(erlang:is_pid(Pid)),
    gen_server:cast(nsime_gbtrees_scheduler, junk),
    Pid ! junk,
    nsime_gbtrees_scheduler:code_change(junk, junk, junk),
    ?assertEqual(nsime_gbtrees_scheduler:stop(), stopped).

create_nsime_event(Time) ->    
    #nsime_event{
        time = Time,
        pid = erlang:self(),
        module = erlang,
        function = date,
        eventid = make_ref()
    }.

