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
-include("nsime_udp_protocol_state.hrl").

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
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_set_get_components(_) ->
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

    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_endpoint_allocation(_) ->
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    EndpointPid1 = nsime_udp_protocol:allocate(ProtocolPid),
    ?assert(is_pid(EndpointPid1)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid1),
        nsime_ipv4_address:get_any()
    ),
    Port1 = nsime_ip_endpoint:get_local_port(EndpointPid1),
    ?assert(is_integer(Port1) and (Port1 =< 65535) and (Port1 >= 49152)),

    Address = {10, 107, 1, 1},
    EndpointPid2 = nsime_udp_protocol:allocate(ProtocolPid, Address),
    ?assert(is_pid(EndpointPid2)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid2),
        Address
    ),
    Port2 = nsime_ip_endpoint:get_local_port(EndpointPid2),
    ?assert(is_integer(Port2) and (Port2 =< 65535) and (Port2 >= 49152)),

    Port3 = case Port2 + 1 == Port1 of
        true ->
            Port2 + 2;
        false ->
            Port2 + 1
    end,
    EndpointPid3 = nsime_udp_protocol:allocate(ProtocolPid, Port3),
    ?assert(is_pid(EndpointPid3)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid3),
        nsime_ipv4_address:get_any()
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointPid3),
        Port3
    ),

    Address2 = {10, 107, 1, 2},
    Port4 = Port3 + 1,
    EndpointPid4 = nsime_udp_protocol:allocate(ProtocolPid, Address2, Port4),
    ?assert(is_pid(EndpointPid4)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid4),
        Address2
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointPid4),
        Port4
    ),

    Address3 = {10, 107, 1, 3},
    Port5 = Port4 + 1,
    PeerAddress = {192, 168, 0, 1},
    PeerPort = 80,
    EndpointPid5 = nsime_udp_protocol:allocate(
        ProtocolPid,
        Address3,
        Port5,
        PeerAddress,
        PeerPort
    ),
    ?assert(is_pid(EndpointPid5)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid5),
        Address3
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointPid5),
        Port5
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_address(EndpointPid5),
        PeerAddress
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_port(EndpointPid5),
        PeerPort
    ),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointPid1), ok),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointPid2), ok),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointPid3), ok),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointPid4), ok),
    ?assertEqual(nsime_udp_protocol:deallocate(ProtocolPid, EndpointPid5), ok),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_send(_) ->
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
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_recv(_) ->
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
        nsime_udp_protocol:recv(ProtocolPid, Packet1, Ipv4Header, undefined),
        rx_csum_failed
    ),
    Ipv4ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(Ipv4ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(Ipv4ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    EndpointPid1 = nsime_udp_protocol:allocate(
        ProtocolPid,
        DestAddress,
        DestPort
    ),
    ?assertEqual(nsime_ip_endpoint:bind_to_netdevice(EndpointPid1, DevicePid1), ok),

    InterfacePid1 = nsime_ipv4_protocol:add_interface(Ipv4ProtocolPid, DevicePid1),
    Mask = {255, 255, 255, 0},
    AddressPid1 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_pid(AddressPid1)),
    AddressPid2 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid1), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid2), ok),

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
        nsime_udp_protocol:recv(ProtocolPid, Packet2, Ipv4Header, InterfacePid1),
        rx_endpoint_unreach
    ),
    EndpointPid2 = nsime_udp_protocol:allocate(
        ProtocolPid,
        DestAddress,
        DestPort,
        SrcAddress,
        SrcPort
    ),
    Ref = make_ref(),
    ReceiveCallback = {
        ?MODULE,
        receive_callback_tester,
        [self(), Ref]
    },
    ?assertEqual(nsime_ip_endpoint:set_receive_callback(EndpointPid2, ReceiveCallback), ok),
    ?assertEqual(nsime_ip_endpoint:bind_to_netdevice(EndpointPid2, DevicePid1), ok),
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assertEqual(
        nsime_udp_protocol:recv(ProtocolPid, Packet2, Ipv4Header, InterfacePid1),
        ok
    ),

    ?assertEqual(nsime_simulator:stop(), stopped),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

test_cast_info_codechange(_) ->
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    gen_server:cast(ProtocolPid, junk),
    ProtocolPid ! junk,
    nsime_udp_protocol:code_change(junk, junk, junk),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped).

%% Helper methods %%

ipv4_down_target_tester(From, Ref, _Packet, _SrcAddress, _DestAddress, _Protocol, _Route) ->
    spawn(fun() -> From ! {ipv4_down_target, Ref} end).

receive_callback_tester(From, Ref, _Packet, _Header, _Port, _Interface) ->
    spawn(fun() -> From ! {receive_callback, Ref} end).
