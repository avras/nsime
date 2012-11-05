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

%% Purpose : Scheduler module based on orddict
%% Author : Saravanan Vijayakumaran

-module(nsime_orddict_scheduler).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").

-export([create/0, is_empty/1, insert/2,
         remove/2, remove_next/1]).

create() ->
    orddict:new().

insert(EventQueue, Event) ->
    orddict:append(nsime_time:value(Event#nsime_event.time), Event, EventQueue).

is_empty(EventQueue) ->
    EventQueue == [].

remove(EventQueue, Event) ->
    TimeValue = nsime_time:value(Event#nsime_event.time),
    case is_empty(EventQueue) of
        false ->
            case orddict:find(TimeValue, EventQueue) of
                error ->
                    none;
                {ok, ExistingEvents} ->
                    NewEvents = lists:delete(Event, ExistingEvents),
                    case length(NewEvents) of
                        0 ->
                            NewEventQueue = orddict:erase(TimeValue, EventQueue),
                            {ok, NewEventQueue};
                        _ ->
                            NewEventQueue = orddict:store(TimeValue, NewEvents, EventQueue),
                            {ok, NewEventQueue}
                    end
            end;
        true ->
            none
    end.

remove_next(EventQueue) ->
    case is_empty(EventQueue) of
        false ->
            {TimeValue, [FirstEvent | RemainingEvents]} = hd(EventQueue),
            case RemainingEvents of
                [] ->
                    {FirstEvent, tl(EventQueue)};
                _ ->
                    NewEventQueue = orddict:store(TimeValue, RemainingEvents, EventQueue),
                    {FirstEvent, NewEventQueue}
            end;
        true ->
            none
    end.
