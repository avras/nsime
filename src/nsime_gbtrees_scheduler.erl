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

-export([create/0, is_empty/1, insert/2,
         remove/2, remove_next/1]).

create() ->
    gb_trees:empty().

insert(EventQueue, Event) ->
    Time = Event#nsime_event.time,
    case gb_trees:lookup(nsime_time:value(Time), EventQueue) of
        none -> 
            gb_trees:insert(nsime_time:value(Time), [Event], EventQueue);
        {value, ExistingEvents} ->
            gb_trees:update(nsime_time:value(Time), [Event | ExistingEvents], EventQueue)
    end.

is_empty(EventQueue) ->
    gb_trees:is_empty(EventQueue).

remove(EventQueue, Event) ->
    Time = Event#nsime_event.time,
    case gb_trees:is_empty(EventQueue) of
        false ->
            case gb_trees:lookup(nsime_time:value(Time), EventQueue) of
                none -> 
                    none;
                {value, ExistingEvents} ->
                    NewEvents = lists:delete(Event, ExistingEvents),
                    case length(NewEvents) of
                        0 -> 
                            NewEventQueue = gb_trees:delete(nsime_time:value(Time), EventQueue),
                            {ok, NewEventQueue};
                        _ ->
                            NewEventQueue = gb_trees:update(nsime_time:value(Time), NewEvents, EventQueue),
                            {ok, NewEventQueue}
                    end
            end;
        true ->
            none
    end.

remove_next(EventQueue) ->
    case gb_trees:is_empty(EventQueue) of
        false ->
            {Time, [FirstEvent | RemainingEvents], NewEventQueue} = gb_trees:take_smallest(EventQueue),
            case RemainingEvents of
                [] ->
                    {FirstEvent, NewEventQueue};
                _ ->
                    NewerEventQueue = gb_trees:insert(Time, RemainingEvents, NewEventQueue),
                    {FirstEvent, NewerEventQueue}
            end;
        true ->
            none
    end.
