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

%% Purpose : Simulator module
%% Author : Saravanan Vijayakumaran
%% Description: Implementation of singleton simulator

-module(nsime_simulator).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_simulator_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, start/1, run/0, parallel_run/0, stop/0, schedule/2,
         schedule_now/1, cancel/1, current_time/0]).

start() ->
    start(gb_trees).

start(SchedulerType) ->
    Scheduler =
    case SchedulerType of
        gb_trees ->
            nsime_gbtrees_scheduler;
        orddict ->
            nsime_orddict_scheduler;
        _ ->
            erlang:error(unsupported_scheduler)
    end,
    gen_server:start({local, ?MODULE}, ?MODULE, Scheduler, []).

stop() ->
    gen_server:call(?MODULE, terminate, infinity).

init(Scheduler) ->
    SimulatorState = #nsime_simulator_state{
        scheduler = Scheduler,
        scheduler_state = Scheduler:create()
    },
    nsime_config:start(),
    nsime_config:disable_checksum(),
    nsime_node_list:start(),
    nsime_channel_list:start(),
    {ok, SimulatorState}.

run() ->
    case gen_server:call(?MODULE, run) of
        {event, Event} ->
            erlang:apply(
                Event#nsime_event.module,
                Event#nsime_event.function,
                Event#nsime_event.arguments
            ),
            ?MODULE:run();
        none ->
            simulation_complete
    end.

parallel_run() ->
    case gen_server:call(?MODULE, parallel_run) of
        {events, EventList} ->
            plists:foreach(
                fun(Event) ->
                    erlang:apply(
                        Event#nsime_event.module,
                        Event#nsime_event.function,
                        Event#nsime_event.arguments
                    )
                end,
                EventList,
                {processes, 5}
            ),
            ?MODULE:parallel_run();
        none ->
            simulation_complete
    end.

schedule(Time, Event = #nsime_event{}) ->
    case nsime_time:is_nsime_time(Time) of
        true ->
            gen_server:call(?MODULE, {schedule, Time, Event});
        false ->
            erlang:error(invalid_argument)
    end.

schedule_now(Event = #nsime_event{}) ->
    schedule({0, sec}, Event).

cancel(Event) ->
    gen_server:call(?MODULE, {cancel, Event}).

current_time() ->
    gen_server:call(?MODULE, current_time).

handle_call({schedule, Time, Event}, _From, State) ->
    EventTime = nsime_time:add(State#nsime_simulator_state.current_time, Time),
    NewEvent = Event#nsime_event{time = EventTime},
    Scheduler = State#nsime_simulator_state.scheduler,
    SchedulerState = State#nsime_simulator_state.scheduler_state,
    NewSchedulerState = Scheduler:insert(SchedulerState, NewEvent),
    NumEvents = State#nsime_simulator_state.num_remaining_events,
    NewState = State#nsime_simulator_state{
        scheduler_state = NewSchedulerState,
        num_remaining_events = NumEvents + 1
    },
    {reply, NewEvent, NewState};

handle_call({cancel, Event}, _From, State) ->
    Scheduler = State#nsime_simulator_state.scheduler,
    SchedulerState = State#nsime_simulator_state.scheduler_state,
    case Scheduler:remove(SchedulerState, Event) of
        {ok, NewSchedulerState} ->
            NumEvents = State#nsime_simulator_state.num_remaining_events,
            NewState = State#nsime_simulator_state{
                scheduler_state = NewSchedulerState,
                num_remaining_events = NumEvents - 1
            },
            {reply, ok, NewState};
        none ->
            {reply, none, State}
    end;

handle_call(run, _From, State) ->
    Scheduler = State#nsime_simulator_state.scheduler,
    SchedulerState = State#nsime_simulator_state.scheduler_state,
    case Scheduler:remove_next(SchedulerState) of
        {Event, NewSchedulerState} ->
            NumEvents = State#nsime_simulator_state.num_remaining_events,
            NumExecutedEvents = State#nsime_simulator_state.num_executed_events,
            NewState = State#nsime_simulator_state{
                scheduler_state = NewSchedulerState,
                current_time = Event#nsime_event.time,
                num_remaining_events = NumEvents - 1,
                num_executed_events = NumExecutedEvents + 1
            },
            {reply, {event, Event}, NewState};
        none ->
            {reply, none, State}
    end;

handle_call(parallel_run, _From, State) ->
    Scheduler = State#nsime_simulator_state.scheduler,
    SchedulerState = State#nsime_simulator_state.scheduler_state,
    case Scheduler:remove_next_simultaneous(SchedulerState) of
        {EarliestEvents, NewSchedulerState} ->
            NumEvents = State#nsime_simulator_state.num_remaining_events,
            NumExecutedEvents = State#nsime_simulator_state.num_executed_events,
            NumEarliestEvents = length(EarliestEvents),
            Event = hd(EarliestEvents),
            NewState = State#nsime_simulator_state{
                scheduler_state = NewSchedulerState,
                current_time = Event#nsime_event.time,
                num_remaining_events = NumEvents - NumEarliestEvents,
                num_executed_events = NumExecutedEvents + NumEarliestEvents
            },
            {reply, {events, EarliestEvents}, NewState};
        none ->
            {reply, none, State}
    end;

handle_call(current_time, _From, State) ->
    {reply, State#nsime_simulator_state.current_time, State};

handle_call(terminate, _From, State) ->
    {stop, normal, simulation_complete, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lists:foreach(
        fun(A) ->
            Pid = erlang:whereis(A),
            case is_pid(Pid) of
                true ->
                    A:stop();
                false ->
                    ok
            end
        end,
        [nsime_node_list, nsime_channel_list, nsime_config]
    ),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
