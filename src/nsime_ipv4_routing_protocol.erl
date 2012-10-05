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

%% Purpose : IPv4 routing protocol module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_routing_protocol).
-author("Saravanan Vijayakumaran").

-export([route_input/9, route_output/4,
         notify_interface_up/2, notify_interface_down/2,
         notify_add_address/3, notify_remove_address/3,
         set_ipv4_protocol/2]).

route_input(
    RoutingProtocolPid,
    Packet,
    Ipv4Header,
    IngressNetdevice,
    UnicastForwardCallback,
    MulticastForwardCallback,
    LocalDeliverCallback,
    ErrorCallback,
    InterfaceList
) ->
    case
    gen_server:call(RoutingProtocolPid, {route_input,
                                         Packet,
                                         Ipv4Header,
                                         IngressNetdevice,
                                         UnicastForwardCallback,
                                         MulticastForwardCallback,
                                         LocalDeliverCallback,
                                         ErrorCallback,
                                         InterfaceList
                                        })
    of
        options_not_supported ->
            erlang:error(options_not_supported);
        false ->
            false;
        true ->
            true
    end.

route_output(
    RoutingProtocolPid,
    Packet,
    Ipv4Header,
    OutputNetdevice
) ->
    gen_server:call(RoutingProtocolPid, {route_output,
                                         Packet,
                                         Ipv4Header,
                                         OutputNetdevice
                                        }).

notify_interface_up(RoutingProtocolPid, InterfacePid) ->
    gen_server:call(RoutingProtocolPid, {notify_interface_up, InterfacePid}).

notify_interface_down(RoutingProtocolPid, InterfacePid) ->
    gen_server:call(RoutingProtocolPid, {notify_interface_down, InterfacePid}).

notify_add_address(RoutingProtocolPid, InterfacePid, InterfaceAddress) ->
    gen_server:call(RoutingProtocolPid, {notify_add_address,
                                         InterfacePid,
                                         InterfaceAddress
                                        }).

notify_remove_address(RoutingProtocolPid, InterfacePid, InterfaceAddress) ->
    gen_server:call(RoutingProtocolPid, {notify_remove_address,
                                         InterfacePid,
                                         InterfaceAddress
                                        }).

set_ipv4_protocol(RoutingProtocolPid, Ipv4ProtocolPid) ->
    gen_server:call(RoutingProtocolPid, {set_ipv4_protocol, Ipv4ProtocolPid}).
