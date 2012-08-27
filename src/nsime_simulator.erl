%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Simulator module
%% Author : Saravanan Vijayakumaran
%% Description: Implementation of singleton simulator

-module(nsime_simulator).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_simulator_state.hrl").

-export([start/0, start/1, init/1, run/0, stop/0]).
-export([schedule/2, cancel/1]).
-export([current_time/0]).
-export([loop/1]).

start() ->
    start(gb_trees).

start(SchedulerType) ->
    Scheduler =
    case SchedulerType of
        gb_trees ->
            nsime_gbtrees_scheduler;
        _ ->
            erlang:error(unsupported_scheduler)
    end,
    register(?MODULE, spawn(?MODULE, init, [Scheduler])),
    nsime_utils:wait_for_registration([nsime_simulator, nsime_gbtrees_scheduler]).

init(Scheduler) ->
    Scheduler:create(),
    SimulatorState = #nsime_simulator_state{
        scheduler = Scheduler
    },
    loop(SimulatorState).

stop() ->
    Ref = make_ref(),
    ?MODULE ! {stop_scheduler, self(), Ref},
    receive
        {ok, Ref} ->
            MonitorRef = erlang:monitor(process, ?MODULE),
            exit(whereis(?MODULE), kill),
            receive
                {'DOWN', MonitorRef, process, {?MODULE, _Node}, Reason} ->
                    Reason
            end
    end.

run() ->
    Ref = make_ref(),
    ?MODULE ! {run, self(), Ref},
    receive
        {ok, Event, Ref} ->
            erlang:apply(
                Event#nsime_event.module,
                Event#nsime_event.function,
                Event#nsime_event.arguments
            ),
            ?MODULE:run();
        {none, Ref} ->
            simulation_complete
    end.


schedule(Time, Event = #nsime_event{}) ->
    case nsime_time:is_nsime_time(Time) of
        true ->
            Ref = make_ref(),
            ?MODULE ! {schedule, self(), Time, Event, Ref},
                receive 
                    {ok, Ref} -> ok
                end;
        false ->
            erlang:error(invalid_argument)
    end.

cancel(Event) ->
    Ref = make_ref(),
    ?MODULE ! {cancel, self(), Event, Ref},
        receive 
            {ok, Ref} -> ok
        end.

current_time() ->
    Ref = make_ref(),
    ?MODULE ! {current_time, self(), Ref},
        receive
            {current_time, Time, Ref} -> Time
        end.


loop(State) ->
    receive
        {schedule, From, Time, Event, Ref} ->
            EventTime = nsime_time:add(State#nsime_simulator_state.current_time, Time),
            NewEvent = Event#nsime_event{time = EventTime},
            Scheduler = State#nsime_simulator_state.scheduler,
            Scheduler:insert(NewEvent),
            NumEvents = State#nsime_simulator_state.num_remaining_events,
            NewState = State#nsime_simulator_state{num_remaining_events = NumEvents + 1},
            From ! {ok, Ref},
            loop(NewState);
        {cancel, From, Event, Ref} ->
            Scheduler = State#nsime_simulator_state.scheduler,
            case Scheduler:remove(Event) of
                ok ->
                    NumEvents = State#nsime_simulator_state.num_remaining_events,
                    NewState = State#nsime_simulator_state{num_remaining_events = NumEvents - 1};
                none ->
                    NewState = State
            end,
            From ! {ok, Ref},
            loop(NewState);
        {run, From, Ref} ->
            Scheduler = State#nsime_simulator_state.scheduler,
            case Scheduler:is_empty() of
                false ->
                    Event = Scheduler:remove_next(),
                    NumEvents = State#nsime_simulator_state.num_remaining_events,
                    NumExecutedEvents = State#nsime_simulator_state.num_executed_events,
                    From ! {ok, Event, Ref},
                    NewState = State#nsime_simulator_state{
                                    num_remaining_events = NumEvents - 1,
                                    num_executed_events = NumExecutedEvents + 1
                    },
                    loop(NewState);
                true ->
                    From ! {none, Ref},
                    loop(State)
            end;
        {current_time, From, Ref} ->
            From ! {current_time, State#nsime_simulator_state.current_time, Ref},
            loop(State);
        {stop_scheduler, From, Ref} ->
            Scheduler = State#nsime_simulator_state.scheduler,
            Scheduler:stop(),
            From ! {ok, Ref},
            loop(State)
    end.
