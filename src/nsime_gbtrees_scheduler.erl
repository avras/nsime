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

%% Purpose : Scheduler module based on gb_trees
%% Author : Saravanan Vijayakumaran

-module(nsime_gbtrees_scheduler).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, stop/0, is_empty/0, insert/1, 
         remove/1, remove_next/0, remove_next_simultaneous/0, get_event_queue/0]).

create() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

insert(Event) ->
    gen_server:call(?MODULE, {insert, Event}).

is_empty() ->
    gen_server:call(?MODULE, is_empty).

remove(Event) ->
    gen_server:call(?MODULE, {remove, Event}).

remove_next() ->
    gen_server:call(?MODULE, remove_next).

remove_next_simultaneous() ->
    gen_server:call(?MODULE, remove_next_simultaneous).

stop() ->
    gen_server:call(?MODULE, terminate).

get_event_queue() ->
    gen_server:call(?MODULE, get_event_queue).

init([]) ->
    EventQueue = gb_trees:empty(),
    {ok, EventQueue}.

handle_call(is_empty, _From, EventQueue) ->
    {reply, gb_trees:is_empty(EventQueue), EventQueue};

handle_call({insert, Event}, _From, EventQueue) ->
    Time = Event#nsime_event.time,
    case gb_trees:lookup(nsime_time:value(Time), EventQueue) of
        none -> 
            NewEventQueue = gb_trees:insert(nsime_time:value(Time), [Event], EventQueue),
            {reply, ok, NewEventQueue};
        {value, ExistingEvents} ->
            NewEventQueue = gb_trees:update(nsime_time:value(Time), [Event | ExistingEvents], EventQueue),
            {reply, ok, NewEventQueue}
    end;

handle_call(get_event_queue, _From, EventQueue) ->
    {reply, EventQueue, EventQueue};

handle_call(remove_next, _From, EventQueue) ->
    case gb_trees:is_empty(EventQueue) of
        false ->
            {Time, [FirstEvent | RemainingEvents], NewEventQueue} = gb_trees:take_smallest(EventQueue),
            case RemainingEvents of 
                [] ->
                    {reply, FirstEvent, NewEventQueue};
                _ ->
                    NewerEventQueue = gb_trees:insert(Time, RemainingEvents, NewEventQueue),
                    {reply, FirstEvent, NewerEventQueue}
            end;
        true ->
            {reply, none, EventQueue}
    end;

handle_call(remove_next_simultaneous, _From, EventQueue) ->
    case gb_trees:is_empty(EventQueue) of
        false ->
            {_Time, EventList, NewEventQueue} = gb_trees:take_smallest(EventQueue),
            {reply, EventList, NewEventQueue};
        true ->
            {reply, none, EventQueue}
    end;

handle_call({remove, Event}, _From, EventQueue) ->
    Time = Event#nsime_event.time,
    case gb_trees:is_empty(EventQueue) of
        false ->
            case gb_trees:lookup(nsime_time:value(Time), EventQueue) of
                none -> 
                    {reply, none, EventQueue};
                {value, ExistingEvents} ->
                    NewEvents = lists:delete(Event, ExistingEvents),
                    case length(NewEvents) of
                        0 -> 
                            NewEventQueue = gb_trees:delete(nsime_time:value(Time), EventQueue),
                            {reply, ok, NewEventQueue};
                        _ ->
                            NewEventQueue = gb_trees:update(nsime_time:value(Time), NewEvents, EventQueue),
                            {reply, ok, NewEventQueue}
                    end
            end;
        true ->
            {reply, none, EventQueue}
    end;

handle_call(terminate, _From, EventQueue) ->
    {stop, normal, stopped, EventQueue}.

handle_cast(_Request, EventQueue) ->
    {noreply, EventQueue}.

handle_info(_Request, EventQueue) ->
    {noreply, EventQueue}.

terminate(_Reason, _EventQueue) ->
    ok.

code_change(_OldVersion, EventQueue, _Extra) ->
    {ok, EventQueue}.
