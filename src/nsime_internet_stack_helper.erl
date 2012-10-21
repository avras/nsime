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

%% Purpose : Internet stack helper module
%% Author : Saravanan Vijayakumaran

-module(nsime_internet_stack_helper).
-author("Saravanan Vijayakumaran").

-export([install/1]).

install(NodePidList) ->
    lists:foreach(
        fun(Node) ->
            Ipv4Protocol = nsime_node:get_object(Node, nsime_ipv4_protocol),
            case is_pid(Ipv4Protocol) of
                true ->
                    erlang:error(ipv4_already_present);
                false ->
                    Ipv4ProtocolPid = nsime_ipv4_protocol:create(),
                    nsime_ipv4_protocol:set_node(Ipv4ProtocolPid, Node),
                    nsime_node:add_object(Node, nsime_ipv4_protocol, Ipv4ProtocolPid),
                    UdpProtocolPid = nsime_udp_protocol:create(),
                    nsime_ipv4_protocol:insert_layer4_protocol(Ipv4ProtocolPid, UdpProtocolPid),
                    nsime_udp_protocol:set_node(UdpProtocolPid, Node),
                    nsime_udp_protocol:set_ipv4_down_target(UdpProtocolPid, {nsime_ipv4_protocol, send, [Ipv4ProtocolPid]}),
                    nsime_node:add_object(Node, nsime_udp_protocol, UdpProtocolPid),
                    ListRoutingPid = nsime_ipv4_list_routing:create(),
                    StaticRoutingPid = nsime_ipv4_static_routing:create(),
                    nsime_ipv4_list_routing:add_routing_protocol(ListRoutingPid, StaticRoutingPid, 0),
                    nsime_ipv4_protocol:set_routing_protocol(Ipv4ProtocolPid, ListRoutingPid)
            end
        end,
        NodePidList
    ),
    ok.
