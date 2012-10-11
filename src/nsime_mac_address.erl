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

%% Purpose : MAC address allocator module
%% Author : Saravanan Vijayakumaran
%% Description: Implementation of singleton MAC address allocator

-module(nsime_mac_address).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_mac_address_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0, allocate/0]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    case lists:member(nsime_mac_address, erlang:registered()) of
        true ->
            gen_server:call(?MODULE, terminate);
        false ->
            stopped
    end.

init([]) ->
    StopTime = {infinity, sec},
    StopEvent = #nsime_event{
        module = ?MODULE,
        function = stop,
        arguments = [],
        eventid = make_ref()
    },
    case lists:member(nsime_simulator, erlang:registered()) of
        false ->
            nsime_simulator:start(),
            nsime_simulator:schedule(StopTime, StopEvent);
        true ->
            nsime_simulator:schedule(StopTime, StopEvent)
    end,
    AllocatorState = #nsime_mac_address_state{},
    {ok, AllocatorState}.

allocate() ->
    case lists:member(nsime_mac_address, erlang:registered()) of
        true ->
            gen_server:call(?MODULE, allocate);
        false ->
            nsime_mac_address:start(),
            gen_server:call(?MODULE, allocate)
    end.

handle_call(allocate, _From, AllocatorState) ->
    NewAddress = AllocatorState#nsime_mac_address_state.current_address + 1,
    NewAllocatorState = AllocatorState#nsime_mac_address_state{
        current_address = NewAddress
    },
    {reply, <<NewAddress:48>>, NewAllocatorState};

handle_call(terminate, _From, AllocatorState) ->
    {stop, normal, stopped, AllocatorState}.

handle_cast(_Request, AllocatorState) ->
    {noreply, AllocatorState}.

handle_info(_Request, AllocatorState) ->
    {noreply, AllocatorState}.

terminate(_Reason, _AllocatorState) ->
    ok.

code_change(_OldVersion, AllocatorState, _Extra) ->
    {ok, AllocatorState}.
