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

%% Purpose : Test module for nsime_udp_protocol
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_protocol_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_ip_endpoint_state.hrl").
-include("nsime_ip_endpoint_demux_state.hrl").
-include("nsime_udp_protocol_state.hrl").
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
            test_endpoint_allocation,
            test_send,
            test_recv,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    ?assertEqual(nsime_simulator:stop(), simulation_complete),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_set_get_components(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),

    NodePid = nsime_node:create(),
    ?assertEqual(nsime_udp_protocol:set_node(ProtocolPid, NodePid), ok),
    ?assertEqual(nsime_udp_protocol:protocol_number(), ?UDP_PROTOCOL_NUMBER),
    ?assertEqual(gen_server:call(ProtocolPid, protocol_number), ?UDP_PROTOCOL_NUMBER),

    SocketPid = nsime_udp_protocol:create_socket(ProtocolPid),
    ?assert(is_pid(SocketPid)),
    ?assertEqual(nsime_udp_protocol:get_sockets(ProtocolPid), [SocketPid]),

    Callback1 = {lists, sort, [1]},
    ?assertEqual(nsime_udp_protocol:set_ipv4_down_target(ProtocolPid, Callback1), ok),
    ?assertEqual(nsime_udp_protocol:get_ipv4_down_target(ProtocolPid), Callback1),
    Callback2 = {lists, sort, [2]},
    ?assertEqual(nsime_udp_protocol:set_ipv6_down_target(ProtocolPid, Callback2), ok),
    ?assertEqual(nsime_udp_protocol:get_ipv6_down_target(ProtocolPid), Callback2),

    ?assertEqual(nsime_simulator:stop(), simulation_complete),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_endpoint_allocation(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    SocketPid = nsime_udp_protocol:create_socket(ProtocolPid),
    ?assert(is_pid(SocketPid)),
    Callbacks = create_endpoint_callbacks(SocketPid),
    EndpointState1 = nsime_udp_protocol:allocate(ProtocolPid, Callbacks),
    ?assert(is_record(EndpointState1, nsime_ip_endpoint_state)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState1),
        nsime_ipv4_address:get_any()
    ),
    Port1 = nsime_ip_endpoint:get_local_port(EndpointState1),
    ?assert(is_integer(Port1) and (Port1 =< 65535) and (Port1 >= 49152)),

    Address = {10, 107, 1, 1},
    EndpointState2 = nsime_udp_protocol:allocate(ProtocolPid, Address, Callbacks),
    ?assert(is_record(EndpointState2, nsime_ip_endpoint_state)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState2),
        Address
    ),
    Port2 = nsime_ip_endpoint:get_local_port(EndpointState2),
    ?assert(is_integer(Port2) and (Port2 =< 65535) and (Port2 >= 49152)),

    Port3 = case Port2 + 1 == Port1 of
        true ->
            Port2 + 2;
        false ->
            Port2 + 1
    end,
    EndpointState3 = nsime_udp_protocol:allocate(ProtocolPid, Port3, Callbacks),
    ?assert(is_record(EndpointState3, nsime_ip_endpoint_state)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState3),
        nsime_ipv4_address:get_any()
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointState3),
        Port3
    ),

    Address2 = {10, 107, 1, 2},
    Port4 = Port3 + 1,
    EndpointState4 = nsime_udp_protocol:allocate(ProtocolPid, Address2, Port4, Callbacks),
    ?assert(is_record(EndpointState4, nsime_ip_endpoint_state)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState4),
        Address2
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointState4),
        Port4
    ),

    Address3 = {10, 107, 1, 3},
    Port5 = Port4 + 1,
    PeerAddress = {192, 168, 0, 1},
    PeerPort = 80,
    EndpointState5 = nsime_udp_protocol:allocate(
        ProtocolPid,
        Address3,
        Port5,
        PeerAddress,
        PeerPort,
        Callbacks
    ),
    ?assert(is_record(EndpointState5, nsime_ip_endpoint_state)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState5),
        Address3
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointState5),
        Port5
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_address(EndpointState5),
        PeerAddress
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_port(EndpointState5),
        PeerPort
    ),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointState1), ok),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointState2), ok),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointState3), ok),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointState4), ok),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointState5), ok),
    ?assertEqual(nsime_simulator:stop(), simulation_complete),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_send(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    Packet = #nsime_packet{data = <<0:32>>, size = 4},
    SrcAddress = {10, 107, 1, 1},
    DestAddress = {192, 168, 0, 1},
    SrcPort = 8080,
    DestPort = 80,
    Route = undefined,
    Ref = make_ref(),
    Callback = {
        ?MODULE,
        ipv4_down_target_tester,
        [self(), Ref]
    },
    ?assertEqual(nsime_udp_protocol:set_ipv4_down_target(ProtocolPid, Callback), ok),
    ?assertEqual(
        nsime_udp_protocol:send(
            ProtocolPid,
            Packet,
            SrcAddress,
            DestAddress,
            SrcPort,
            DestPort
        ),
        ok
    ),
    receive
        {ipv4_down_target, Ref} ->
            ok
    end,
    ?assertEqual(
        nsime_udp_protocol:send(
            ProtocolPid,
            Packet,
            SrcAddress,
            DestAddress,
            SrcPort,
            DestPort,
            Route
        ),
        ok
    ),
    receive
        {ipv4_down_target, Ref} ->
            ok
    end,
    ?assertEqual(nsime_simulator:stop(), simulation_complete),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_recv(_) ->
    nsime_simulator:start(),
    nsime_config:enable_checksum(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    ?assertEqual(
        nsime_udp_protocol:recv_icmp(ProtocolPid, junk, junk, junk, junk, junk, junk, junk, junk),
        ok
    ),

    Packet1 = #nsime_packet{data = <<1:160>>, size = 20},
    SrcAddress = {10, 107, 1, 1},
    DestAddress = {192, 168, 0, 1},
    SrcPort = 8080,
    DestPort = 80,
    Ipv4Header = #nsime_ipv4_header{
        source_address = SrcAddress,
        destination_address = DestAddress,
        ttl = 32
    },
    ?assertEqual(
        nsime_udp_protocol:recv(ProtocolPid, Packet1, Ipv4Header, #nsime_ipv4_interface_state{}),
        rx_csum_failed
    ),
    Ipv4ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(Ipv4ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(Ipv4ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    SocketPid = nsime_udp_protocol:create_socket(ProtocolPid),
    Callbacks = create_endpoint_callbacks(SocketPid),
    EndpointState1 = nsime_udp_protocol:allocate(
        ProtocolPid,
        DestAddress,
        DestPort,
        Callbacks
    ),
    EndpointState2 = nsime_ip_endpoint:bind_to_netdevice(EndpointState1, DevicePid1),

    InterfaceState1 = nsime_ipv4_protocol:add_interface(Ipv4ProtocolPid, DevicePid1),
    Mask = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    AddressState2 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    InterfaceState2 = nsime_ipv4_interface:add_address(InterfaceState1, AddressState1),
    InterfaceState3 = nsime_ipv4_interface:add_address(InterfaceState2, AddressState2),

    Data = <<0:96>>,
    Checksum = nsime_udp_header:calculate_checksum(
        Data,
        SrcAddress,
        DestAddress,
        SrcPort,
        DestPort,
        20,
        ?UDP_PROTOCOL_NUMBER
    ),
    Packet2 = #nsime_packet{
        data = <<SrcPort:16, DestPort:16, 20:16, Checksum:16, Data/binary>>,
        size = 20
    },
    ?assertEqual(
        nsime_udp_protocol:recv(ProtocolPid, Packet2, Ipv4Header, InterfaceState3),
        rx_endpoint_unreach
    ),
    EndpointState3 = nsime_udp_protocol:allocate(
        ProtocolPid,
        DestAddress,
        DestPort,
        SrcAddress,
        SrcPort,
        Callbacks
    ),
%   ?assertEqual(
%       nsime_udp_protocol:recv(ProtocolPid, Packet2, Ipv4Header, InterfaceState3),
%       rx_ok
%   ),

    ?assertEqual(nsime_simulator:stop(), simulation_complete),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_cast_info_codechange(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    gen_server:cast(ProtocolPid, junk),
    ProtocolPid ! junk,
    nsime_udp_protocol:code_change(junk, junk, junk),
    ?assertEqual(nsime_simulator:stop(), simulation_complete),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

%% Helper methods %%

ipv4_down_target_tester(From, Ref, _Packet, _SrcAddress, _DestAddress, _Protocol, _Route) ->
    spawn(fun() -> From ! {ipv4_down_target, Ref} end).

receive_callback_tester(From, Ref, _Packet, _Header, _Port, _Interface) ->
    spawn(fun() -> From ! {receive_callback, Ref} end).

create_endpoint_callbacks(UdpSocketPid) ->
    {
        {
            nsime_udp_socket,
            forward_up,
            [UdpSocketPid]
        },
        {
            nsime_udp_socket,
            forward_icmp,
            [UdpSocketPid]
        },
        {
            nsime_udp_socket,
            destroy_endpoint,
            [UdpSocketPid]
        }
    }.
