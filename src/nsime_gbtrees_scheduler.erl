%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Scheduler module based on gb_trees
%% Author : Saravanan Vijayakumaran

-module(nsime_gbtrees_scheduler).
-author("Saravanan Vijayakumaran").

-export([create/0, stop/0, loop/1, insert/1, is_empty/0, remove/1, remove_next/0, get_event_queue/0]).

-include("nsime_event.hrl").

-behaviour(nsime_scheduler).

create() ->
    EventQueue = gb_trees:empty(),
    register(?MODULE, spawn(?MODULE, loop, [EventQueue])).

insert(Event = #nsime_event{}) ->
    ?MODULE ! {insert, self(), Event},
    receive
        ok -> ok
    end.

is_empty() ->
    ?MODULE ! {is_empty, self()},
    receive
      {is_empty, IsEmpty} ->
          IsEmpty
    end.

remove(Event=#nsime_event{}) ->
    ?MODULE ! {remove, self(), Event},
    receive
        ok -> 
            ok;
        none ->
            none
    end.

remove_next() ->
    ?MODULE ! {remove_next, self()},
    receive
        {event, Event} ->
            Event;
        none ->
            none
    end.

stop() ->
    Ref = erlang:monitor(process, ?MODULE),
    exit(whereis(?MODULE), kill),
    receive
        {'DOWN', Ref, process, {?MODULE, _Node}, Reason} ->
            Reason
    end.

get_event_queue() ->
    ?MODULE ! {get_event_queue, self()},
    receive 
      EventQueue -> EventQueue
    end.

loop(EventQueue) ->
    receive
        {is_empty, From} ->
            From ! {is_empty, gb_trees:is_empty(EventQueue)},
            loop(EventQueue);
        {insert, From, Event = #nsime_event{time = Time}} ->
            case gb_trees:lookup(Time, EventQueue) of
                none -> 
                    NewEventQueue = gb_trees:insert(Time, [Event], EventQueue),
                    From ! ok,
                    loop(NewEventQueue);
                {value, ExistingEvents} ->
                    NewEventQueue = gb_trees:update(Time, [Event | ExistingEvents], EventQueue),
                    From ! ok,
                    loop(NewEventQueue)
            end;
        {get_event_queue, From} ->
            From ! EventQueue,
            loop(EventQueue);
        {remove_next, From} -> 
            case gb_trees:is_empty(EventQueue) of
                false ->
                    {Time, [FirstEvent | RemainingEvents], NewEventQueue} = gb_trees:take_smallest(EventQueue),
                    case RemainingEvents of 
                        [] ->
                            From ! {event, FirstEvent},
                            loop(NewEventQueue);
                        _ ->
                            NewerEventQueue = gb_trees:insert(Time, RemainingEvents, NewEventQueue),
                            From ! {event, FirstEvent},
                            loop(NewerEventQueue)
                    end;
                true ->
                    From ! none,
                    loop(EventQueue)
            end;
        {remove, From, Event = #nsime_event{time = Time}} -> 
            case gb_trees:is_empty(EventQueue) of
                false ->
                    case gb_trees:lookup(Time, EventQueue) of
                        none -> 
                            From ! none,
                            loop(EventQueue);
                        {value, ExistingEvents} ->
                            NewEvents = lists:delete(Event, ExistingEvents),
                            case length(NewEvents) of
                                0 -> 
                                    NewEventQueue = gb_trees:delete(Time, EventQueue),
                                    From ! ok,
                                    loop(NewEventQueue);
                                _ ->
                                    NewEventQueue = gb_trees:update(Time, NewEvents, EventQueue),
                                    From ! ok,
                                    loop(NewEventQueue)
                            end
                    end;
                true ->
                    From ! none,
                    loop(EventQueue)
            end
    end.
