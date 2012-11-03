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

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").

-export([create/0, set_device/2, get_device/1,
         set_arp_cache/2, get_arp_cache/1,
         set_metric/2, get_metric/1,
         is_up/1, is_down/1, set_up/1, set_down/1,
         is_forwarding/1, set_forwarding/2,
         send/4, add_address/2, remove_address/2,
         get_address_list/1]).

create() ->
    #nsime_ipv4_interface_state{
        interfaceid = make_ref()
    }.

set_device(InterfaceState, DevicePid) ->
    InterfaceState#nsime_ipv4_interface_state{
        device = DevicePid
    }.

get_device(InterfaceState) ->
    InterfaceState#nsime_ipv4_interface_state.device.

set_arp_cache(InterfaceState, ArpCachePid) ->
    InterfaceState#nsime_ipv4_interface_state{
        arp_cache = ArpCachePid
    }.

get_arp_cache(InterfaceState) ->
    InterfaceState#nsime_ipv4_interface_state.arp_cache.

set_metric(InterfaceState, Metric) ->
    InterfaceState#nsime_ipv4_interface_state{
        metric = Metric
    }.

get_metric(InterfaceState) ->
    InterfaceState#nsime_ipv4_interface_state.metric.

is_up(InterfaceState) ->
    InterfaceState#nsime_ipv4_interface_state.interface_up.

is_down(InterfaceState) ->
    not(InterfaceState#nsime_ipv4_interface_state.interface_up).

set_up(InterfaceState) ->
    InterfaceState#nsime_ipv4_interface_state{
        interface_up = true
    }.

set_down(InterfaceState) ->
    InterfaceState#nsime_ipv4_interface_state{
        interface_up = false
    }.

is_forwarding(InterfaceState) ->
    InterfaceState#nsime_ipv4_interface_state.forwarding.

set_forwarding(InterfaceState, Forwarding) ->
    InterfaceState#nsime_ipv4_interface_state{
        forwarding = Forwarding
    }.

send(InterfaceState, Packet, DestAddress, IPv4ProtocolPid) ->
    case InterfaceState#nsime_ipv4_interface_state.interface_up of
        false ->
            interface_down;
        true ->
            Device = InterfaceState#nsime_ipv4_interface_state.device,
            case (nsime_netdevice:get_device_type(Device) == nsime_loopback_netdevice) of
                true ->
                    nsime_netdevice:send(
                        Device,
                        Packet,
                        nsime_netdevice:get_broadcast_address(Device),
                        nsime_ipv4_protocol:protocol_number()
                    );
                false ->
                    AddressList = InterfaceState#nsime_ipv4_interface_state.address_list,
                    MatchingAddress =
                    case
                    lists:filter(
                        fun(A) ->
                            nsime_ipv4_interface_address:get_local_address(A) == DestAddress
                        end,
                        AddressList
                    )
                    of
                        [] ->
                            undefined;
                        [FirstMatch|_] ->
                            FirstMatch
                    end,
                    case is_record(MatchingAddress, nsime_ipv4_interface_address_state) of
                        true ->
                            Event = #nsime_event{
                                module = nsime_ipv4_protocol,
                                function = recv,
                                arguments = [
                                    IPv4ProtocolPid,
                                    Device,
                                    Packet,
                                    nsime_ipv4_protocol:protocol_number(),
                                    nsime_netdevice:get_broadcast_address(Device),
                                    DestAddress,
                                    packet_host
                                ],
                                eventid = make_ref()
                            },
                            nsime_simulator:schedule_now(Event);
                        false ->
                            case nsime_netdevice:needs_arp(Device) of
                                true ->
                                    ArpCache = InterfaceState#nsime_ipv4_interface_state.arp_cache,
                                    {HardwareAddress, Found} =
                                    case {nsime_ipv4_address:is_broadcast(DestAddress), nsime_ipv4_address:is_multicast(DestAddress)} of
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
                                    case Found of
                                        true ->
                                            nsime_netdevice:send(
                                                Device,
                                                Packet,
                                                HardwareAddress,
                                                nsime_ipv4_protocol:protocol_number()
                                            );
                                        false ->
                                            address_unresolved
                                    end;
                                false ->
                                    nsime_netdevice:send(
                                        Device,
                                        Packet,
                                        nsime_netdevice:get_broadcast_address(Device),
                                        nsime_ipv4_protocol:protocol_number()
                                    )
                            end
                    end
            end
    end.

add_address(InterfaceState, AddressState) ->
    AddressList = InterfaceState#nsime_ipv4_interface_state.address_list,
    InterfaceState#nsime_ipv4_interface_state{
        address_list = [AddressState | AddressList]
    }.

remove_address(InterfaceState, AddressState) ->
    AddressList = InterfaceState#nsime_ipv4_interface_state.address_list,
    InterfaceState#nsime_ipv4_interface_state{
        address_list = lists:delete(AddressState, AddressList)
    }.

get_address_list(InterfaceState) ->
    InterfaceState#nsime_ipv4_interface_state.address_list.
