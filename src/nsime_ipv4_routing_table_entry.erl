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

%% Purpose : IPv4 routing table entry module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_routing_table_entry).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_ipv4_routing_table_entry.hrl").

-export([is_host/1, is_network/1, is_default/1, is_gateway/1,
         create_host_route/2, create_host_route/3,
         create_network_route/3, create_network_route/4,
         create_default_route/2]).

is_host(#nsime_ipv4_routing_table_entry{network_mask = DestNetworkMask}) ->
    DestNetworkMask == nsime_ipv4_mask:get_ones().

is_network(#nsime_ipv4_routing_table_entry{network_mask = DestNetworkMask}) ->
    DestNetworkMask =/= nsime_ipv4_mask:get_ones().

is_default(#nsime_ipv4_routing_table_entry{destination = Destination}) ->
    Destination == nsime_ipv4_address:get_zero().

is_gateway(#nsime_ipv4_routing_table_entry{gateway = Gateway}) ->
    Gateway =/= nsime_ipv4_address:get_zero().

create_host_route(Destination, InterfaceIndex) ->
    #nsime_ipv4_routing_table_entry{
        destination = Destination,
        network_mask = nsime_ipv4_mask:get_ones(),
        gateway = nsime_ipv4_address:get_zero(),
        interface_index = InterfaceIndex
    }.

create_host_route(Destination, NextHop, InterfaceIndex) ->
    #nsime_ipv4_routing_table_entry{
        destination = Destination,
        network_mask = nsime_ipv4_mask:get_ones(),
        gateway = NextHop,
        interface_index = InterfaceIndex
    }.

create_network_route(Network, NetworkMask, InterfaceIndex) ->
    #nsime_ipv4_routing_table_entry{
        destination = Network,
        network_mask = NetworkMask,
        gateway = nsime_ipv4_address:get_zero(),
        interface_index = InterfaceIndex
    }.

create_network_route(Network, NetworkMask, NextHop, InterfaceIndex) ->
    #nsime_ipv4_routing_table_entry{
        destination = Network,
        network_mask = NetworkMask,
        gateway = NextHop,
        interface_index = InterfaceIndex
    }.

create_default_route(NextHop, InterfaceIndex) ->
    #nsime_ipv4_routing_table_entry{
        destination = nsime_ipv4_address:get_zero(),
        network_mask = nsime_ipv4_mask:get_ones(),
        gateway = NextHop,
        interface_index = InterfaceIndex
    }.


