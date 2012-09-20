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

%% Purpose : IPv4 interface module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_interface).
-author("Saravanan Vijayakumaran").

-include("nsime_ipv4_interface_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, set_node/2, destroy/1,
         set_device/2, get_device/1,
         set_arp_cache/2, get_arp_cache/1,
         set_metric/2, get_metric/1,
         is_up/1, is_down/1, set_up/1, set_down/1,
         is_forwarding/1, set_forwarding/2,
         send/3, add_address/2, remove_address/2,
         get_address/2, get_address_list/1]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(InterfacePid) ->
    gen_server:call(InterfacePid, terminate).

set_node(InterfacePid, NodePid) ->
    gen_server:call(InterfacePid, {set_node, NodePid}).

set_device(InterfacePid, DevicePid) ->
    gen_server:call(InterfacePid, {set_device, DevicePid}).

get_device(InterfacePid) ->
    gen_server:call(InterfacePid, get_device).

set_arp_cache(InterfacePid, ArpCachePid) ->
    gen_server:call(InterfacePid, {set_arp_cache, ArpCachePid}).

get_arp_cache(InterfacePid) ->
    gen_server:call(InterfacePid, get_arp_cache).

set_metric(InterfacePid, Metric) ->
    gen_server:call(InterfacePid, {set_metric, Metric}).

get_metric(InterfacePid) ->
    gen_server:call(InterfacePid, get_metric).

is_up(InterfacePid) ->
    gen_server:call(InterfacePid, is_up).

is_down(InterfacePid) ->
    gen_server:call(InterfacePid, is_down).

set_up(InterfacePid) ->
    gen_server:call(InterfacePid, set_up).

set_down(InterfacePid) ->
    gen_server:call(InterfacePid, set_down).

is_forwarding(InterfacePid) ->
    gen_server:call(InterfacePid, is_forwarding).

set_forwarding(InterfacePid, Forwarding) ->
    gen_server:call(InterfacePid, {set_forwarding, Forwarding}).

send(InterfacePid, Packet, DestAddress) ->
    gen_server:call(InterfacePid, {send, Packet, DestAddress}).

add_address(InterfacePid, Address) ->
    gen_server:call(InterfacePid, {add_address, Address}).

remove_address(InterfacePid, Address) ->
    gen_server:call(InterfacePid, {remove_address, Address}).

get_address(InterfacePid, Index) ->
    gen_server:call(InterfacePid, {get_address, Index}).

get_address_list(InterfacePid) ->
    gen_server:call(InterfacePid, get_address_list).

init([]) ->
    InterfaceState = #nsime_ipv4_interface_state{},
    {ok, InterfaceState}.

handle_call({set_node, NodePid}, _From, InterfaceState) ->
    NewInterfaceState = InterfaceState#nsime_ipv4_interface_state{
        node = NodePid
    },
    {reply, ok, NewInterfaceState};

handle_call({set_device, DevicePid}, _From, InterfaceState) ->
    NewInterfaceState = InterfaceState#nsime_ipv4_interface_state{
        device = DevicePid
    },
    {reply, ok, NewInterfaceState};

handle_call(get_device, _From, InterfaceState) ->
    DevicePid = InterfaceState#nsime_ipv4_interface_state.device,
    {reply, DevicePid, InterfaceState};

handle_call({set_arp_cache, ArpCachePid}, _From, InterfaceState) ->
    NewInterfaceState = InterfaceState#nsime_ipv4_interface_state{
        arp_cache = ArpCachePid
    },
    {reply, ok, NewInterfaceState};

handle_call(get_arp_cache, _From, InterfaceState) ->
    ArpCachePid = InterfaceState#nsime_ipv4_interface_state.arp_cache,
    {reply, ArpCachePid, InterfaceState};

handle_call({set_metric, Metric}, _From, InterfaceState) ->
    NewInterfaceState = InterfaceState#nsime_ipv4_interface_state{
        metric = Metric
    },
    {reply, ok, NewInterfaceState};

handle_call(get_metric, _From, InterfaceState) ->
    Metric = InterfaceState#nsime_ipv4_interface_state.metric,
    {reply, Metric, InterfaceState};

handle_call(is_up, _From, InterfaceState) ->
    Up = InterfaceState#nsime_ipv4_interface_state.interface_up,
    {reply, Up == true, InterfaceState};

handle_call(is_down, _From, InterfaceState) ->
    Up = InterfaceState#nsime_ipv4_interface_state.interface_up,
    {reply, Up == false, InterfaceState};

handle_call(set_up, _From, InterfaceState) ->
    NewInterfaceState = InterfaceState#nsime_ipv4_interface_state{
        interface_up = true
    },
    {reply, ok, NewInterfaceState};

handle_call(set_down, _From, InterfaceState) ->
    NewInterfaceState = InterfaceState#nsime_ipv4_interface_state{
        interface_up = false
    },
    {reply, ok, NewInterfaceState};

handle_call(is_forwarding, _From, InterfaceState) ->
    Forwarding = InterfaceState#nsime_ipv4_interface_state.forwarding,
    {reply, Forwarding == true, InterfaceState};

handle_call({set_forwarding, Forwarding}, _From, InterfaceState) ->
    NewInterfaceState = InterfaceState#nsime_ipv4_interface_state{
        forwarding = Forwarding
    },
    {reply, ok, NewInterfaceState};

handle_call({send, Packet, DestAddress}, _From, InterfaceState) ->
    case InterfaceState#nsime_ipv4_interface_state.interface_up of
        false ->
            {reply, interface_down, InterfaceState};
        true ->
            Device = InterfaceState#nsime_ipv4_interface_state.device,

            case (nsime_netdevice:get_device_type(Device) == nsime_loopback_netdevice) of
                true ->
                    nsime_netdevice:send(
                        Packet,
                        nsime_netdevice:get_broadcast_address(Device),
                        nsime_ipv4_protocol:protocol_number()
                    ),
                    {reply, ok, InterfaceState};
                false ->
                    AddressList = InterfaceState#nsime_ipv4_interface_state.address_list,
                    [MatchingAddress|_] = lists:filter(
                        fun(A) ->
                            nsime_ipv4_interface_address:get_local_address(A) == DestAddress
                        end,
                        AddressList
                    ),
                    case is_pid(MatchingAddress) of
                        false ->
                            IPv4Protocol = nsime_node:get_pid(nsime_ipv4_protocol),
                            nsime_ipv4_protocol:receive_packet(
                                IPv4Protocol,
                                Device,
                                Packet,
                                nsime_ipv4_protocol:protocol_number(),
                                nsime_device:get_broadcast_address(Device),
                                DestAddress,
                                packet_host
                            ),
                            {reply, ok, InterfaceState};
                        true ->
                            case nsime_netdevice:needs_arp(Device) of
                                true ->
                                    ArpCache = InterfaceState#nsime_ipv4_interface_state.arp_cache,
                                    {HardwareAddress, Found} = case {nsime_ipv4_address:is_broadcast(DestAddress),
                                          nsime_ipv4_address:is_multicast(DestAddress)} of
                                        {true, false} ->
                                            {nsime_netdevice:get_broadcast_address(Device), true};
                                        {false, true} ->
                                            {nsime_netdevice:get_multicast_address(Device), true};
                                        {false, false} ->
                                            case lists:any(
                                                fun(A) ->
                                                    Mask = nsime_ipv4_interface_address:get_mask(A),
                                                    nsime_ipv4_address:is_subnet_directed_broadcast(
                                                        DestAddress,
                                                        Mask
                                                    )
                                                end,
                                                AddressList
                                            ) of
                                                false ->
                                                    case nsime_arp_protocol:lookup(
                                                        Packet,
                                                        DestAddress,
                                                        Device,
                                                        ArpCache
                                                    ) of
                                                        {true, Address} ->
                                                            {Address, true};
                                                        false ->
                                                            {undefined, false}
                                                    end;
                                                true ->
                                                    {nsime_netdevice:get_broadcast_address(Device), true}
                                            end
                                    end,
                                    if
                                        Found ->
                                            nsime_netdevice:send(
                                                Device,
                                                Packet,
                                                HardwareAddress,
                                                nsime_ipv4_protocol:protocol_number()
                                            ),
                                            {reply, ok, InterfaceState};
                                        true ->
                                            {reply, address_unresolved, InterfaceState}
                                    end;
                                false ->
                                    nsime_netdevice:send(
                                        Device,
                                        Packet,
                                        nsime_netdevice:get_broadcast_address(Device),
                                        nsime_ipv4_protocol:protocol_number()
                                    ),
                                    {reply, ok, InterfaceState}
                            end
                    end
            end
    end;

handle_call({add_address, Address}, _From, InterfaceState) ->
    AddressList = InterfaceState#nsime_ipv4_interface_state.address_list,
    NewInterfaceState = InterfaceState#nsime_ipv4_interface_state{
        address_list = [Address | AddressList]
    },
    {reply, ok, NewInterfaceState};

handle_call({remove_address, Address}, _From, InterfaceState) ->
    AddressList = InterfaceState#nsime_ipv4_interface_state.address_list,
    NewInterfaceState = InterfaceState#nsime_ipv4_interface_state{
        address_list = lists:delete(Address, AddressList)
    },
    {reply, ok, NewInterfaceState};

handle_call({get_address, Index}, _From, InterfaceState) ->
    AddressList = InterfaceState#nsime_ipv4_interface_state.address_list,
    {reply, lists:nth(Index, AddressList), InterfaceState};

handle_call(get_address_list, _From, InterfaceState) ->
    AddressList = InterfaceState#nsime_ipv4_interface_state.address_list,
    {reply, AddressList, InterfaceState};

handle_call(terminate, _From, InterfaceState) ->
    {stop, normal, stopped, InterfaceState}.

handle_cast(_Request, InterfaceState) ->
    {noreply, InterfaceState}.

handle_info(_Request, InterfaceState) ->
    {noreply, InterfaceState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, InterfaceState, _Extra) ->
    {ok, InterfaceState}.
