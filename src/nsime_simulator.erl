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

-export([start/0, start/1, run/0, stop/0]).
-export([schedule/2, cancel/1]).
-export([current_time/0]).
-export([loop/1]).

start() ->
    register(?MODULE, spawn(?MODULE, loop, [[]])).

%% Argument will be the type of scheduler(map, list, heap)
start(_) ->
    ok.

run() ->
    ok.

stop() ->
    ?MODULE ! shutdown.

schedule(Time, Event = #nsime_event{pid=From}) ->
    ?MODULE ! {schedule, Time, From, Event},
        receive 
            {ok, State} -> {ok, State}
        end.

cancel(Event) ->
    ?MODULE ! {cancel, self(), Event},
        receive 
            {ok, State} -> {ok, State}
        end.

current_time() ->
    ok.

loop(State) ->
    receive
        {schedule, Time, From, Event} ->
            NewState = [{Time, Event} | State], 
            From ! {ok, NewState},
            loop(NewState);
        {cancel, From, Event} ->
            NewState = lists:delete(Event, State), 
            From ! {ok, NewState},
            loop(NewState);
        shutdown ->
            exit(shutdown)
    end.
