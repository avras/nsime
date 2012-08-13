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

-include("nsime_event.hrl").
-include("nsime_simulator_state.hrl").

-export([start/0, start/1, run/0, stop/0]).
-export([schedule/2, cancel/1]).
-export([current_time/0]).
-export([loop/1]).

start() ->
    SimulatorState = #nsime_simulator_state{
                          current_time = 0,
                          scheduler = nsime_gbtrees_scheduler,
                          num_remaining_events = 0,
                          num_executed_events = 0,
                          stopped = false
                     },
    Scheduler = SimulatorState#nsime_simulator_state.scheduler,
    Scheduler:create(),
    register(?MODULE, spawn(?MODULE, loop, [SimulatorState])).

%% Argument will be the type of scheduler(map, list, heap)
start(_) ->
    ok.

run() ->
    Ref = make_ref(),
    ?MODULE ! {run, self(), Ref},
    receive
        {ok, Ref} -> 
            ?MODULE:run();
        {none, Ref} ->
            ?MODULE:stop()
    end.


stop() ->
    Ref = erlang:monitor(process, ?MODULE),
    exit(whereis(?MODULE), kill),
    receive
        {'DOWN', Ref, process, {?MODULE, _Node}, Reason} ->
            Reason
    end.

schedule(Time, Event = #nsime_event{}) ->
    Ref = make_ref(),
    ?MODULE ! {schedule, self(), Time, Event, Ref},
        receive 
            {ok, Ref} -> ok
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
            EventTime = State#nsime_simulator_state.current_time + Time,
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
                    erlang:apply(Event#nsime_event.module, Event#nsime_event.function, Event#nsime_event.arguments),
                    From ! {ok, Ref},
                    loop(State#nsime_simulator_state{num_remaining_events = NumEvents - 1, num_executed_events = NumExecutedEvents + 1});
                true ->
                    From ! {none, Ref},
                    loop(State)
            end;
        {current_time, From, Ref} ->
            From ! {current_time, State#nsime_simulator_state.current_time, Ref},
            loop(State)
    end.
