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

%% Purpose : Test module for nsime_udp_socket
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_socket_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_ip_endpoint_state.hrl").
-include("nsime_udp_socket_state.hrl").
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
            test_bind,
            test_close_and_shutdown,
            test_connect_and_listen,
            test_forward_icmp,
            test_send_error_conditions,
            test_send_broadcast_address,
            test_send_unicast_address,
            test_send_to,
            test_recv,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    SocketPid = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid)),
    ?assertEqual(nsime_udp_socket:destroy(SocketPid), stopped).

test_set_get_components(_) ->
    nsime_simulator:start(),
    SocketPid = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid)),

    NodePid = nsime_node:create(),
    ?assertEqual(nsime_udp_socket:set_node(SocketPid, NodePid), ok),
    ?assertEqual(nsime_udp_socket:get_node(SocketPid), NodePid),

    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    ?assertEqual(nsime_udp_socket:set_udp_protocol(SocketPid, ProtocolPid), ok),

    ?assertEqual(nsime_udp_socket:get_socket_error(SocketPid), error_noterror),
    ?assertEqual(nsime_udp_socket:get_socket_type(SocketPid), sock_dgram),
    ?assertEqual(
        nsime_udp_socket:get_transmit_available(SocketPid),
        ?MAX_IPv4_UDP_DATAGRAM_SIZE
    ),
    ?assertEqual(nsime_udp_socket:get_received_available(SocketPid), 0),

    ?assertEqual(nsime_udp_socket:get_receive_buffer_size(SocketPid), infinity),
    BufferSize = 10,
    ?assertEqual(nsime_udp_socket:set_receive_buffer_size(SocketPid, BufferSize), ok),
    ?assertEqual(nsime_udp_socket:get_receive_buffer_size(SocketPid), BufferSize),

    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    ?assertEqual(nsime_udp_socket:bind_to_netdevice(SocketPid, DevicePid1), ok),

    ?assertEqual(nsime_udp_socket:set_allow_broadcast(SocketPid, true), ok),
    ?assert(nsime_udp_socket:get_allow_broadcast(SocketPid)),
    ?assertEqual(nsime_udp_socket:set_allow_broadcast(SocketPid, false), ok),
    ?assertNot(nsime_udp_socket:get_allow_broadcast(SocketPid)),

    Callback1 = {lists, sort, [1]},
    ?assertEqual(nsime_udp_socket:set_drop_trace_callback(SocketPid, Callback1), ok),
    Callback2 = {lists, sort, [2]},
    ?assertEqual(nsime_udp_socket:set_icmp_callback(SocketPid, Callback2), ok),

    ?assertEqual(nsime_udp_socket:multicast_join_group(SocketPid, junk, junk), ok),
    ?assertEqual(nsime_udp_socket:multicast_leave_group(SocketPid, junk, junk), ok),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped),
    ?assertEqual(nsime_udp_socket:destroy(SocketPid), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_bind(_) ->
    nsime_simulator:start(),
    SocketPid = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid)),

    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    ?assertEqual(nsime_udp_socket:set_udp_protocol(SocketPid, ProtocolPid), ok),

    ?assertEqual(nsime_udp_socket:destroy_endpoint(SocketPid), ok),
    ?assertEqual(nsime_udp_socket:bind(SocketPid), ok),
    ?assertEqual(nsime_udp_socket:destroy_endpoint(SocketPid), ok),
    ?assertEqual(nsime_udp_socket:bind(SocketPid, {nsime_ipv4_address:get_any(), 0}), ok),
    ?assertEqual(nsime_udp_socket:destroy_endpoint(SocketPid), ok),
    ?assertEqual(nsime_udp_socket:bind(SocketPid, {nsime_ipv4_address:get_any(), 80}), ok),
    ?assertEqual(nsime_udp_socket:destroy_endpoint(SocketPid), ok),
    ?assertEqual(nsime_udp_socket:bind(SocketPid, {{10, 107, 1, 1}, 0}), ok),
    ?assertEqual(nsime_udp_socket:destroy_endpoint(SocketPid), ok),
    ?assertEqual(nsime_udp_socket:bind(SocketPid, {{10, 107, 1, 1}, 80}), ok),
    ?assertEqual(nsime_udp_socket:destroy_endpoint(SocketPid), ok),

    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped),
    ?assertEqual(nsime_udp_socket:destroy(SocketPid), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_close_and_shutdown(_) ->
    SocketPid1 = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid1)),
    SocketPid2 = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid2)),

    ?assertEqual(nsime_udp_socket:close(SocketPid1), ok),
    ?assertEqual(nsime_udp_socket:close(SocketPid1), error_badf),

    ?assertEqual(nsime_udp_socket:shutdown_send(SocketPid2), ok),
    ?assertEqual(nsime_udp_socket:shutdown_receive(SocketPid2), ok),
    ?assertEqual(nsime_udp_socket:close(SocketPid2), error_badf),

    ?assertEqual(nsime_udp_socket:destroy(SocketPid2), stopped),
    ?assertEqual(nsime_udp_socket:destroy(SocketPid1), stopped).

test_connect_and_listen(_) ->
    SocketPid1 = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid1)),

    ?assertEqual(nsime_udp_socket:connect(SocketPid1, {{10,107,1,1}, 80}), ok),
    ?assertEqual(nsime_udp_socket:listen(SocketPid1), error_opnotsupp),
    ?assertEqual(nsime_udp_socket:get_socket_error(SocketPid1), error_opnotsupp),

    ?assertEqual(nsime_udp_socket:destroy(SocketPid1), stopped).

test_forward_icmp(_) ->
    SocketPid1 = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid1)),

    ?assertEqual(nsime_udp_socket:forward_icmp(SocketPid1, junk, junk, junk, junk, junk), none),
    Ref = make_ref(),
    IcmpCallback = {
        ?MODULE,
        icmp_callback_tester,
        [self(), Ref]
    },
    ?assertEqual(nsime_udp_socket:set_icmp_callback(SocketPid1, IcmpCallback), ok),
    ?assertEqual(nsime_udp_socket:forward_icmp(SocketPid1, junk, junk, junk, junk, junk), ok),
    receive
        {icmp_callback, Ref} ->
            ok
    end,

    ?assertEqual(nsime_udp_socket:destroy(SocketPid1), stopped).

test_send_error_conditions(_) ->
    SocketPid1 = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid1)),

    Packet = #nsime_packet{
        data = <<0:32>>,
        size = 4
    },
    ?assertEqual(nsime_udp_socket:send(SocketPid1, Packet, 0), error_notconn),
    ?assertEqual(nsime_udp_socket:connect(SocketPid1, {{10,107,1,1}, 80}), ok),
    ?assertEqual(nsime_udp_socket:shutdown_send(SocketPid1), ok),
    ?assertEqual(nsime_udp_socket:send(SocketPid1, Packet, 0), error_shutdown),
    ?assertEqual(nsime_udp_socket:destroy(SocketPid1), stopped).

test_send_broadcast_address(_) ->
    nsime_simulator:start(),
    SocketPid2 = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid2)),
    Address1 = nsime_ipv4_address:get_broadcast(),
    ?assertEqual(nsime_udp_socket:connect(SocketPid2, {Address1, 80}), ok),
    UdpProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(UdpProtocolPid)),
    ?assertEqual(nsime_udp_socket:set_udp_protocol(SocketPid2, UdpProtocolPid), ok),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_udp_socket:set_node(SocketPid2, NodePid), ok),
    Ipv4ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(Ipv4ProtocolPid)),
    ?assertEqual(nsime_ipv4_protocol:set_node(Ipv4ProtocolPid, NodePid), ok),
     Packet = #nsime_packet{
         data = <<0:32>>,
         size = 4
     },
    ?assertEqual(
        nsime_udp_socket:send(
            SocketPid2,
            Packet#nsime_packet{size = ?MAX_IPv4_UDP_DATAGRAM_SIZE + 1},
            0
        ),
        error_msgsize
    ),
    ?assertEqual(nsime_udp_socket:send(SocketPid2, Packet, 0), error_opnotsupp),
    ?assertEqual(nsime_udp_socket:set_allow_broadcast(SocketPid2, true), ok),
    ?assertEqual(nsime_udp_socket:send(SocketPid2, Packet, 0), error_noroutetohost),

    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    ?assertEqual(nsime_udp_socket:bind_to_netdevice(SocketPid2, DevicePid1), ok),
    InterfaceState1 = nsime_ipv4_protocol:add_interface(Ipv4ProtocolPid, DevicePid1),
    ?assert(is_record(InterfaceState1, nsime_ipv4_interface_state)),
    ?assertEqual(nsime_udp_socket:send(SocketPid2, Packet, 0), error_noroutetohost),
    Address2 = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:create(Address2, Mask),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    ?assertEqual(
        nsime_ipv4_protocol:add_interface_address(
            Ipv4ProtocolPid,
            nsime_ipv4_interface:get_id(InterfaceState1),
            AddressState1
            ),
            ok
    ),

    Routing = nsime_ipv4_static_routing:create(),
    ?assertEqual(nsime_ipv4_protocol:set_routing_protocol(Ipv4ProtocolPid, Routing), ok),
    Ref = make_ref(),
    DownTargetCallback = {
        ?MODULE,
        ipv4_down_target_tester,
        [self(), Ref]
    },
    ?assertEqual(nsime_udp_protocol:set_ipv4_down_target(UdpProtocolPid, DownTargetCallback), ok),
    ?assertEqual(nsime_udp_socket:send(SocketPid2, Packet, 0), Packet#nsime_packet.size),
    receive
        {ipv4_down_target, Ref} ->
            ok
    end,
    ?assertEqual(nsime_udp_socket:send(SocketPid2, Packet, 0), Packet#nsime_packet.size),
    ?assertEqual(nsime_udp_socket:destroy(SocketPid2), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_send_unicast_address(_) ->
    nsime_simulator:start(),
    SocketPid2 = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid2)),
    Address1 = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    ?assertEqual(nsime_udp_socket:connect(SocketPid2, {Address1, 80}), ok),
    UdpProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(UdpProtocolPid)),
    ?assertEqual(nsime_udp_socket:set_udp_protocol(SocketPid2, UdpProtocolPid), ok),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_udp_socket:set_node(SocketPid2, NodePid), ok),
    Ipv4ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(Ipv4ProtocolPid)),
    ?assertEqual(nsime_ipv4_protocol:set_node(Ipv4ProtocolPid, NodePid), ok),
    Packet = #nsime_packet{
         data = <<0:32>>,
         size = 4
    },
    ?assertEqual(nsime_udp_socket:send(SocketPid2, Packet, 0), error_noroutetohost),

    {nsime_ipv4_static_routing, RoutingState} = nsime_ipv4_static_routing:create(),
    ?assertEqual(nsime_udp_socket:send(SocketPid2, Packet, 0), error_noroutetohost),

    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    ?assertEqual(nsime_udp_socket:bind_to_netdevice(SocketPid2, DevicePid1), ok),
    InterfaceState1 = nsime_ipv4_protocol:add_interface(Ipv4ProtocolPid, DevicePid1),
    ?assert(is_record(InterfaceState1, nsime_ipv4_interface_state)),
    InterfaceState2 = nsime_ipv4_interface:set_device(InterfaceState1, DevicePid1),
    InterfaceId2 = nsime_ipv4_interface:get_id(InterfaceState2),
    RoutingState1 = nsime_ipv4_static_routing:add_host_route(
        RoutingState,
        Address1,
        InterfaceId2,
        0
    ),
    ?assertEqual(nsime_ipv4_protocol:set_routing_protocol(Ipv4ProtocolPid, {nsime_ipv4_static_routing, RoutingState1}), ok),
    AddressState1 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(Ipv4ProtocolPid, InterfaceId2, AddressState1), ok),

    Ref = make_ref(),
    DownTargetCallback = {
        ?MODULE,
        ipv4_down_target_tester,
        [self(), Ref]
    },
    ?assertEqual(nsime_udp_protocol:set_ipv4_down_target(UdpProtocolPid, DownTargetCallback), ok),
    ?assertEqual(nsime_udp_socket:send(SocketPid2, Packet, 0), Packet#nsime_packet.size),
    receive
        {ipv4_down_target, Ref} ->
            ok
    end,

    ?assertEqual(nsime_udp_socket:destroy(SocketPid2), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_send_to(_) ->
    nsime_simulator:start(),
    SocketPid1 = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid1)),
    Packet = #nsime_packet{
        data = <<0:32>>,
        size = 4
    },
    Address = {10, 107, 1, 1},
    Port = 80,
    ?assertEqual(nsime_udp_socket:connect(SocketPid1, {Address, Port}), ok),
    UdpProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(UdpProtocolPid)),
    ?assertEqual(nsime_udp_socket:set_udp_protocol(SocketPid1, UdpProtocolPid), ok),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_udp_socket:set_node(SocketPid1, NodePid), ok),
    Ipv4ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(Ipv4ProtocolPid)),
    ?assertEqual(nsime_ipv4_protocol:set_node(Ipv4ProtocolPid, NodePid), ok),
    ?assertEqual(nsime_udp_socket:send_to(SocketPid1, Packet, junk, {Address, Port}), error_noroutetohost),
    ?assertEqual(nsime_udp_socket:send_to(SocketPid1, Packet, junk, {Address, Port}), error_noroutetohost),
    ?assertEqual(nsime_udp_socket:destroy(SocketPid1), stopped),
    ?assertEqual(nsime_node:destroy(NodePid), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_recv(_) ->
    nsime_simulator:start(),
    SocketPid1 = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid1)),
    ?assertEqual(nsime_udp_socket:recv(SocketPid1, junk, junk), none),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_udp_socket:set_node(SocketPid1, NodePid), ok),
    Ipv4ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(Ipv4ProtocolPid)),
    ?assertEqual(nsime_ipv4_protocol:set_node(Ipv4ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    ?assertEqual(nsime_udp_socket:bind_to_netdevice(SocketPid1, DevicePid1), ok),
    InterfaceState1 = nsime_ipv4_protocol:add_interface(Ipv4ProtocolPid, DevicePid1),
    ?assert(is_record(InterfaceState1, nsime_ipv4_interface_state)),
    InterfaceState2 = nsime_ipv4_interface:set_device(InterfaceState1, DevicePid1),

    Packet = #nsime_packet{
        data = <<0:32>>,
        size = 4
    },
    SrcAddress = {10, 107, 1, 1},
    Ipv4Header = #nsime_ipv4_header{source_address = SrcAddress},
    Port = 80,
    ReceiveCallback = {none, none, none},
    ?assertEqual(nsime_udp_socket:set_receive_callback(SocketPid1, ReceiveCallback), ok),
    ?assertEqual(nsime_udp_socket:forward_up(SocketPid1, Packet, Ipv4Header, Port, InterfaceState2), ok),
    ?assert(is_record(nsime_udp_socket:recv(SocketPid1, Packet#nsime_packet.size, junk), nsime_packet)),
    ?assertEqual(nsime_udp_socket:forward_up(SocketPid1, Packet, Ipv4Header, Port, InterfaceState2), ok),
    ?assertEqual(nsime_udp_socket:recv(SocketPid1, Packet#nsime_packet.size - 1, junk), none),
    ?assertEqual(nsime_udp_socket:forward_up(SocketPid1, Packet, Ipv4Header, Port, InterfaceState2), ok),
    ?assertEqual(nsime_udp_socket:recv_from(SocketPid1, Packet#nsime_packet.size - 1, junk), none),
    {Packet1, {SrcAddress, Port}} = nsime_udp_socket:recv_from(SocketPid1, Packet#nsime_packet.size, junk),
    ?assert(is_record(Packet1, nsime_packet)),

    Ref = make_ref(),
    DropCallback = {
        ?MODULE,
        drop_trace_tester,
        [self(), Ref]
    },
    ?assertEqual(nsime_udp_socket:set_drop_trace_callback(SocketPid1, DropCallback), ok),
    ?assertEqual(nsime_udp_socket:set_receive_buffer_size(SocketPid1, 2), ok),
    ?assertEqual(nsime_udp_socket:forward_up(SocketPid1, Packet, Ipv4Header, Port, InterfaceState2), dropped),
    receive
        {drop_trace_callback, Ref} ->
            ok
    end,

    ?assertEqual(nsime_udp_socket:shutdown_receive(SocketPid1), ok),
    ?assertEqual(nsime_udp_socket:forward_up(SocketPid1, Packet, Ipv4Header, Port, InterfaceState2), none),

    ?assertEqual(nsime_udp_socket:destroy(SocketPid1), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_cast_info_codechange(_) ->
    SocketPid = nsime_udp_socket:create(),
    ?assert(is_pid(SocketPid)),
    gen_server:cast(SocketPid, junk),
    SocketPid ! junk,
    nsime_udp_socket:code_change(junk, junk, junk),
    ?assertEqual(nsime_udp_socket:destroy(SocketPid), stopped).

%% Helper methods %%

icmp_callback_tester(From, Ref, _Source, _TTL, _Type, _Code, _Info) ->
    spawn(fun() -> From ! {icmp_callback, Ref} end).

ipv4_down_target_tester(From, Ref, _Packet, _SrcAddress, _DestAddress, _Protocol, _Route) ->
    spawn(fun() -> From ! {ipv4_down_target, Ref} end).

drop_trace_tester(From, Ref, _Packet) ->
    spawn(fun() -> From ! {drop_trace_callback, Ref} end).
