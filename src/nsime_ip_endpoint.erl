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

%% Purpose : IP endpoint module
%% Author : Saravanan Vijayakumaran

-module(nsime_ip_endpoint).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_ip_endpoint_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, create/1, create/2, destroy/1,
         get_local_address/1, set_local_address/2,
         get_local_port/1, set_local_port/2,
         set_peer/3, get_peer_address/1, get_peer_port/1,
         bind_to_netdevice/2, get_bound_netdevice/1,
         set_receive_callback/2, set_icmp_callback/2,
         set_destroy_callback/2, forward_up/5, forward_icmp/6]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

create(EndpointState = #nsime_ip_endpoint_state{}) ->
    {ok, Pid} = gen_server:start(?MODULE, EndpointState, []),
    Pid.

create(Address, Port) ->
    {ok, Pid} = gen_server:start(?MODULE, {Address, Port}, []),
    Pid.

destroy(EndpointPid) ->
    gen_server:call(EndpointPid, terminate).

get_local_address(EndpointPid) ->
    gen_server:call(EndpointPid, get_local_address).

set_local_address(EndpointPid, Address) ->
    gen_server:call(EndpointPid, {set_local_address, Address}).

get_local_port(EndpointPid) ->
    gen_server:call(EndpointPid, get_local_port).

set_local_port(EndpointPid, Port) ->
    gen_server:call(EndpointPid, {set_local_port, Port}).

set_peer(EndpointPid, Address, Port) ->
    gen_server:call(EndpointPid, {set_peer, Address, Port}).

get_peer_address(EndpointPid) ->
    gen_server:call(EndpointPid, get_peer_address).

get_peer_port(EndpointPid) ->
    gen_server:call(EndpointPid, get_peer_port).

bind_to_netdevice(EndpointPid, DevicePid) ->
    gen_server:call(EndpointPid, {bind_to_netdevice, DevicePid}).

get_bound_netdevice(EndpointPid) ->
    gen_server:call(EndpointPid, get_bound_netdevice).

set_receive_callback(EndpointPid, Callback) ->
    gen_server:call(EndpointPid, {set_receive_callback, Callback}).

set_icmp_callback(EndpointPid, Callback) ->
    gen_server:call(EndpointPid, {set_icmp_callback, Callback}).

set_destroy_callback(EndpointPid, Callback) ->
    gen_server:call(EndpointPid, {set_destroy_callback, Callback}).

forward_up(EndpointPid, Packet, Header, Port, Interface) ->
    gen_server:call(EndpointPid, {forward_up, Packet, Header, Port, Interface}).

forward_icmp(EndpointPid, Source, TTL, Type, Code, Info) ->
    gen_server:call(
        EndpointPid, 
        {forward_icmp, Source, TTL, Type, Code, Info}
    ).

init([]) ->
    EndpointState = #nsime_ip_endpoint_state{},
    {ok, EndpointState};

init(EndpointState = #nsime_ip_endpoint_state{}) ->
    {ok, EndpointState};

init({Address, Port}) ->
    EndpointState = #nsime_ip_endpoint_state{
        local_address = Address,
        local_port = Port,
        peer_address = nsime_ipv4_address:get_any(),
        peer_port = 0
    },
    {ok, EndpointState}.

handle_call(get_local_address, _From, EndpointState) ->
    Address = EndpointState#nsime_ip_endpoint_state.local_address,
    {reply, Address, EndpointState};

handle_call({set_local_address, Address}, _From, EndpointState) ->
    NewEndpointState = EndpointState#nsime_ip_endpoint_state{local_address = Address},
    {reply, ok, NewEndpointState};

handle_call(get_local_port, _From, EndpointState) ->
    Port = EndpointState#nsime_ip_endpoint_state.local_port,
    {reply, Port, EndpointState};

handle_call({set_local_port, Port}, _From, EndpointState) ->
    NewEndpointState = EndpointState#nsime_ip_endpoint_state{local_port = Port},
    {reply, ok, NewEndpointState};

handle_call({set_peer, Address, Port}, _From, EndpointState) ->
    NewEndpointState = EndpointState#nsime_ip_endpoint_state{
        peer_address = Address,
        peer_port = Port
    },
    {reply, ok, NewEndpointState};

handle_call(get_peer_address, _From, EndpointState) ->
    Address = EndpointState#nsime_ip_endpoint_state.peer_address,
    {reply, Address, EndpointState};

handle_call(get_peer_port, _From, EndpointState) ->
    Port = EndpointState#nsime_ip_endpoint_state.peer_port,
    {reply, Port, EndpointState};

handle_call({bind_to_netdevice, DevicePid}, _From, EndpointState) ->
    NewEndpointState = EndpointState#nsime_ip_endpoint_state{
        bound_netdevice = DevicePid
    },
    {reply, ok, NewEndpointState};

handle_call(get_bound_netdevice, _From, EndpointState) ->
    DevicePid = EndpointState#nsime_ip_endpoint_state.bound_netdevice,
    {reply, DevicePid, EndpointState};

handle_call({set_receive_callback, Callback}, _From, EndpointState) ->
    NewEndpointState = EndpointState#nsime_ip_endpoint_state{
        receive_callback = Callback
    },
    {reply, ok, NewEndpointState};

handle_call({set_icmp_callback, Callback}, _From, EndpointState) ->
    NewEndpointState = EndpointState#nsime_ip_endpoint_state{
        icmp_callback = Callback
    },
    {reply, ok, NewEndpointState};

handle_call({set_destroy_callback, Callback}, _From, EndpointState) ->
    NewEndpointState = EndpointState#nsime_ip_endpoint_state{
        destroy_callback = Callback
    },
    {reply, ok, NewEndpointState};

handle_call({forward_up, Packet, Header, Port, Interface}, _From, EndpointState) ->
    {Module, Function, Arguments} = EndpointState#nsime_ip_endpoint_state.receive_callback,
    Event = #nsime_event{
        module = Module,
        function = Function,
        arguments = lists:flatten([Arguments | [Packet, Header, Port, Interface]]),
        eventid = make_ref()
    },
    nsime_simulator:schedule_now(Event),
    {reply, ok, EndpointState};

handle_call({forward_icmp, Source, TTL, Type, Code, Info}, _From, EndpointState) ->
    {Module, Function, Arguments} 
        = EndpointState#nsime_ip_endpoint_state.icmp_callback,
    Event = #nsime_event{
        module = Module,
        function = Function,
        arguments = lists:flatten([Arguments | [Source, TTL, Type, Code, Info]]),
        eventid = make_ref()
    },
    nsime_simulator:schedule_now(Event),
    {reply, ok, EndpointState};

handle_call(terminate, _From, EndpointState) ->
    {stop, normal, stopped, EndpointState}.

handle_cast(_Request, EndpointState) ->
    {noreply, EndpointState}.

handle_info(_Request, EndpointState) ->
    {noreply, EndpointState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, EndpointState, _Extra) ->
    {ok, EndpointState}.
