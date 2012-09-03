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

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, start/1, run/0, stop/0, schedule/2,
         schedule_now/1, cancel/1, current_time/0]).

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
    gen_server:start({local, ?MODULE}, ?MODULE, Scheduler, []).

stop() ->
    gen_server:call(?MODULE, terminate).

init(Scheduler) ->
    Scheduler:create(),
    SimulatorState = #nsime_simulator_state{
        scheduler = Scheduler
    },
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
    Scheduler:insert(NewEvent),
    NumEvents = State#nsime_simulator_state.num_remaining_events,
    NewState = State#nsime_simulator_state{num_remaining_events = NumEvents + 1},
    {reply, NewEvent, NewState};

handle_call({cancel, Event}, _From, State) ->
    Scheduler = State#nsime_simulator_state.scheduler,
    case Scheduler:remove(Event) of
        ok ->
            NumEvents = State#nsime_simulator_state.num_remaining_events,
            NewState = State#nsime_simulator_state{num_remaining_events = NumEvents - 1},
            {reply, ok, NewState};
        none ->
            {reply, none, State}
    end;

handle_call(run, _From, State) ->
    Scheduler = State#nsime_simulator_state.scheduler,
    case Scheduler:is_empty() of
        false ->
            Event = Scheduler:remove_next(),
            NumEvents = State#nsime_simulator_state.num_remaining_events,
            NumExecutedEvents = State#nsime_simulator_state.num_executed_events,
            NewState = State#nsime_simulator_state{
                            current_time = Event#nsime_event.time,
                            num_remaining_events = NumEvents - 1,
                            num_executed_events = NumExecutedEvents + 1
            },
            {reply, {event, Event}, NewState};
        true ->
            {reply, none, State}
    end;

handle_call(current_time, _From, State) ->
    {reply, State#nsime_simulator_state.current_time, State};

handle_call(terminate, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Scheduler = State#nsime_simulator_state.scheduler,
    Scheduler:stop(),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
