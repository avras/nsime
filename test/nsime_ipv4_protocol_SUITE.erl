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

%% Purpose : Test module for nsime_ipv4_protocol
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_protocol_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_packet.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_ipv4_route.hrl").
-include("nsime_ipv4_protocol_state.hrl").
-include("nsime_icmpv4_protocol_state.hrl").
-include("nsime_udp_protocol_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
            test_route_input_error,
            test_is_destination,
            test_ip_forward_no_ttl_error,
            test_ip_forward_with_ttl_error,
            test_send_with_header,
            test_recv,
            test_send,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_set_get_components(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    ?assertEqual(nsime_ipv4_protocol:protocol_number(), ?IPv4_PROTOCOL_NUMBER),
    ?assertEqual(gen_server:call(ProtocolPid, protocol_number), ?IPv4_PROTOCOL_NUMBER),
    DevicePid1 = nsime_ptp_netdevice:create(),
    InterfacePid1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    ?assert(is_pid(InterfacePid1)),
    ?assertEqual(nsime_ipv4_protocol:set_up(ProtocolPid, InterfacePid1), ok),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfacePid1), ok),
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assertEqual(nsime_ipv4_protocol:set_routing_protocol(ProtocolPid, RoutingPid), ok),
    ?assertEqual(nsime_ipv4_protocol:get_routing_protocol(ProtocolPid), RoutingPid),
    ?assertEqual(nsime_ipv4_protocol:create_raw_socket(ProtocolPid), ok),
    ?assertEqual(nsime_ipv4_protocol:delete_raw_socket(ProtocolPid, undefined), ok),
    Layer4ProtPid1 = nsime_udp_protocol:create(),
    Layer4ProtPid2 = nsime_icmpv4_protocol:create(),
    ?assertEqual(nsime_ipv4_protocol:insert_layer4_protocol(ProtocolPid, Layer4ProtPid1), ok),
    ?assertEqual(nsime_ipv4_protocol:insert_layer4_protocol(ProtocolPid, Layer4ProtPid2), ok),
    ?assertEqual(nsime_ipv4_protocol:get_layer4_protocol(ProtocolPid, ?UDP_PROTOCOL_NUMBER), Layer4ProtPid1),
    ?assertEqual(nsime_ipv4_protocol:get_layer4_protocol(ProtocolPid, ?ICMPv4_PROTOCOL_NUMBER), Layer4ProtPid2),
    ?assertEqual(nsime_ipv4_protocol:remove_layer4_protocol(ProtocolPid, Layer4ProtPid1), ok),
    ?assertEqual(nsime_ipv4_protocol:set_default_ttl(ProtocolPid, 64), ok),
    ?assertEqual(nsime_ipv4_protocol:get_interface_list(ProtocolPid), [InterfacePid1]),
    Address1 = {10, 107, 1, 1},
    AddressPid1 = nsime_ipv4_interface_address:create(),
    ?assert(is_pid(AddressPid1)),
    ?assertEqual(nsime_ipv4_interface_address:set_local_address(AddressPid1, Address1), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid1), ok),
    DevicePid2 = nsime_ptp_netdevice:create(),
    InterfacePid2 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    ?assert(is_pid(InterfacePid2)),
    ?assertEqual(nsime_ipv4_protocol:get_interface_for_address(ProtocolPid, Address1), InterfacePid1),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid2, AddressPid1), ok),
    ?assert(
        (nsime_ipv4_protocol:get_interface_for_address(ProtocolPid, Address1) == InterfacePid1)
        or
        (nsime_ipv4_protocol:get_interface_for_address(ProtocolPid, Address1) == InterfacePid2)
    ),
    ?assertEqual(nsime_ipv4_protocol:get_interface_for_device(ProtocolPid, DevicePid1),  InterfacePid1),
    ?assertEqual(nsime_ipv4_interface:remove_address(InterfacePid2, AddressPid1), ok),
    Address2 = {10, 107, 10, 20},
    Mask = {255, 255, 0, 0},
    Address3 = {10, 107, 1, 1},
    AddressPid2 = nsime_ipv4_interface_address:create(),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface_address:set_local_address(AddressPid2, Address3), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid2, AddressPid2), ok),
    InterfacePid3 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    ?assert(
        (nsime_ipv4_protocol:get_interface_for_prefix(ProtocolPid, Address2, Mask) == InterfacePid1)
        or
        (nsime_ipv4_protocol:get_interface_for_prefix(ProtocolPid, Address2, Mask) == InterfacePid2)
    ),
    ?assert(
        (nsime_ipv4_protocol:get_interface_for_device(ProtocolPid, DevicePid2) == InterfacePid2)
        or
        (nsime_ipv4_protocol:get_interface_for_device(ProtocolPid, DevicePid2) == InterfacePid3)
    ),
    ?assertEqual(nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid1), [AddressPid1]),
    AddressPid3 = nsime_ipv4_interface_address:create(),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfacePid1, AddressPid3), ok),
    ?assert(
        lists:member(
            AddressPid1,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid1)
        )
     ),
    ?assert(
        lists:member(
            AddressPid3,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid1)
        )
     ),
    ?assertEqual(nsime_ipv4_protocol:remove_interface_address(ProtocolPid, InterfacePid1, AddressPid3), ok),
    ?assert(
        lists:member(
            AddressPid1,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid1)
        )
     ),
    ?assertNot(
        lists:member(
            AddressPid3,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid1)
        )
     ),

    ?assertEqual(nsime_ipv4_protocol:select_source_address(ProtocolPid, undefined, undefined, undefined), ok),

    Metric = 43,
    ?assertEqual(nsime_ipv4_protocol:set_metric(ProtocolPid, InterfacePid1, Metric), ok),
    ?assertEqual(nsime_ipv4_protocol:get_metric(ProtocolPid, InterfacePid1), Metric),

    ?assertEqual(
        nsime_ipv4_protocol:get_mtu(ProtocolPid, InterfacePid1),
        nsime_ptp_netdevice:get_mtu(DevicePid1)
    ),

    ?assertNot(nsime_ipv4_protocol:is_up(ProtocolPid, InterfacePid1)),
    ?assertEqual(nsime_ipv4_protocol:set_up(ProtocolPid, InterfacePid1), ok),
    ?assert(nsime_ipv4_protocol:is_up(ProtocolPid, InterfacePid1)),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfacePid1), ok),
    ?assertNot(nsime_ipv4_protocol:is_up(ProtocolPid, InterfacePid1)),

    ?assertEqual(nsime_ipv4_protocol:set_forwarding(ProtocolPid, InterfacePid1, false), ok),
    ?assertNot(nsime_ipv4_protocol:is_forwarding(ProtocolPid, InterfacePid1)),
    ?assertEqual(nsime_ipv4_protocol:set_forwarding(ProtocolPid, InterfacePid1, true), ok),
    ?assert(nsime_ipv4_protocol:is_forwarding(ProtocolPid, InterfacePid1)),

    ?assertEqual(nsime_ipv4_protocol:get_netdevice(ProtocolPid, InterfacePid1), DevicePid1),
    ?assertEqual(nsime_ipv4_protocol:set_weak_es_model(ProtocolPid, true), ok),
    ?assert(nsime_ipv4_protocol:get_weak_es_model(ProtocolPid)),
    ?assertEqual(nsime_ipv4_protocol:set_weak_es_model(ProtocolPid, false), ok),
    ?assertNot(nsime_ipv4_protocol:get_weak_es_model(ProtocolPid)),
    ?assertEqual(
        nsime_ipv4_protocol:set_fragment_expiration_timeout(
            ProtocolPid,
            {1, sec}
        ),
        ok
    ),

    Callback = {erlang, date, []},
    ?assertEqual(nsime_ipv4_protocol:set_transmit_trace(ProtocolPid, Callback), ok),
    ?assertEqual(nsime_ipv4_protocol:set_receive_trace(ProtocolPid, Callback), ok),
    ?assertEqual(nsime_ipv4_protocol:set_drop_trace(ProtocolPid, Callback), ok),
    ?assertEqual(nsime_ipv4_protocol:set_send_outgoing_trace(ProtocolPid, Callback), ok),
    ?assertEqual(nsime_ipv4_protocol:set_unicast_forward_trace(ProtocolPid, Callback), ok),
    ?assertEqual(nsime_ipv4_protocol:set_local_deliver_trace(ProtocolPid, Callback), ok),

    ?assertEqual(nsime_ipv4_interface:destroy(InterfacePid1), stopped),
    ?assertEqual(nsime_ipv4_interface:destroy(InterfacePid2), stopped),
    ?assertEqual(nsime_ipv4_interface:destroy(InterfacePid3), stopped),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid1), stopped),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid2), stopped),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped),
    ?assertEqual(nsime_udp_protocol:destroy(Layer4ProtPid1), stopped),
    ?assertEqual(nsime_udp_protocol:destroy(Layer4ProtPid2), stopped),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_route_input_error(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    Ref = make_ref(),
    Callback = {
        ?MODULE,
        drop_trace_tester,
        [self(), Ref]
    },
    ?assertEqual(nsime_ipv4_protocol:set_drop_trace(ProtocolPid, Callback), ok),
    ?assertEqual(
        nsime_ipv4_protocol:route_input_error(
            ProtocolPid,
            #nsime_packet{},
            #nsime_ipv4_header{}
        ),
        ok
    ),
    receive
        {drop_route_error, Ref} ->
            ok
    end,
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_is_destination(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    InterfacePid1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    Address1 = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressPid1 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_pid(AddressPid1)),
    AddressPid2 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid1), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid2), ok),
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address1, InterfacePid1)),
    Address2 = nsime_ipv4_address:get_subnet_directed_broadcast(Address1, Mask),
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address2, InterfacePid1)),
    Address3 = nsime_ipv4_address:get_broadcast(),
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address3, InterfacePid1)),
    Address4 = {225, 107, 1, 1},
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address4, InterfacePid1)),
    Address5 = {10, 250, 1, 1},
    ?assertEqual(nsime_ipv4_protocol:set_weak_es_model(ProtocolPid, false), ok),
    ?assertNot(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address5, InterfacePid1)),
    DevicePid2 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid2)),
    InterfacePid2 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    InterfacePid3 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    AddressPid3 = nsime_ipv4_interface_address:create(Address5, Mask),
    ?assert(is_pid(AddressPid3)),
    AddressPid4 = nsime_ipv4_interface_address:create(Address5, Mask),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid2, AddressPid3), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid2, AddressPid4), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid3, AddressPid3), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid3, AddressPid4), ok),
    ?assertEqual(nsime_ipv4_protocol:set_weak_es_model(ProtocolPid, true), ok),
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address5, InterfacePid1)),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_ip_forward_no_ttl_error(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid2 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid2)),
    nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    InterfacePid1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    DevicePid3 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid3)),
    nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid3),

    Route = #nsime_ipv4_route{
        output_device = DevicePid1
    },
    Ipv4Header = #nsime_ipv4_header{
        source_address = {10, 107, 1, 1},
        destination_address = {192, 168, 1, 250},
        ttl = 32
    },
    Packet = #nsime_packet{data = <<0:32>>, size = 4},
    Ref1 = make_ref(),
    UnicastCallback = {
        ?MODULE,
        unicast_forward_tester,
        [self(), Ref1]
    },
    ?assertEqual(nsime_ipv4_protocol:set_unicast_forward_trace(ProtocolPid, UnicastCallback), ok),
    Ref2 = make_ref(),
    DropCallback = {
        ?MODULE,
        drop_trace_tester,
        [self(), Ref2]
    },
    ?assertEqual(nsime_ipv4_protocol:set_drop_trace(ProtocolPid, DropCallback), ok),
    ?assertEqual(
        nsime_ipv4_protocol:ip_forward(
            ProtocolPid,
            Route,
            Packet,
            Ipv4Header
        ),
        ok
    ),
    receive
        {unicast_forward_trace, Ref1} ->
            ok
    end,
    receive
        {drop_route_error, Ref2} ->
            ok
    end,
    ?assertEqual(nsime_ipv4_protocol:set_up(ProtocolPid, InterfacePid1), ok),
    Ref3 = make_ref(),
    TransmitCallback = {
        ?MODULE,
        transmit_trace_tester,
        [self(), Ref3]
    },
    ?assertEqual(nsime_ipv4_protocol:set_transmit_trace(ProtocolPid, TransmitCallback), ok),
    Address1 = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressPid1 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_pid(AddressPid1)),
    AddressPid2 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid1), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid2), ok),
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assertEqual(
        nsime_ipv4_protocol:ip_forward(
            ProtocolPid,
            Route,
            Packet,
            Ipv4Header
        ),
        ok
    ),
    receive
        {unicast_forward_trace, Ref1} ->
            ok
    end,
    receive
        {transmit_trace, Ref3} ->
            ok
    end,
    Size = nsime_ptp_netdevice:get_mtu(Route#nsime_ipv4_route.output_device) + 1,
    BigPacket = #nsime_packet{data = <<0:(Size*8)>>, size = Size},
    ?assertError(
        ipv4_fragmentation_not_supported,
        nsime_ipv4_protocol:ip_forward(
            ProtocolPid,
            Route,
            BigPacket,
            Ipv4Header
        )
    ),
    NewRoute = Route#nsime_ipv4_route{gateway = nsime_ipv4_address:get_zero()},
    ?assertEqual(
        nsime_ipv4_protocol:ip_forward(
            ProtocolPid,
            NewRoute,
            Packet,
            Ipv4Header
        ),
        ok
    ),
    receive
        {unicast_forward_trace, Ref1} ->
            ok
    end,
    receive
        {transmit_trace, Ref3} ->
            ok
    end,
    ?assertError(
        ipv4_fragmentation_not_supported,
        nsime_ipv4_protocol:ip_forward(
            ProtocolPid,
            NewRoute,
            BigPacket,
            Ipv4Header
        )
    ),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfacePid1), ok),
    ?assertEqual(
        nsime_ipv4_protocol:ip_forward(
            ProtocolPid,
            NewRoute,
            Packet,
            Ipv4Header
        ),
        ok
    ),
    receive
        {unicast_forward_trace, Ref1} ->
            ok
    end,
    receive
        {drop_route_error, Ref2} ->
            ok
    end,
    ?assertEqual(nsime_simulator:stop(), stopped),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_ip_forward_with_ttl_error(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),

    Route = #nsime_ipv4_route{
        output_device = DevicePid1
    },
    Ipv4Header = #nsime_ipv4_header{
        source_address = {10, 107, 1, 1},
        destination_address = {192, 168, 1, 250},
        ttl = 1
    },
    Packet = #nsime_packet{data = <<0:32>>, size = 4},
    Ref1 = make_ref(),
    DropCallback = {
        ?MODULE,
        drop_trace_tester,
        [self(), Ref1]
    },
    ?assertEqual(nsime_ipv4_protocol:set_drop_trace(ProtocolPid, DropCallback), ok),
    ?assertError(
        icmpv4_protocol_not_found,
        nsime_ipv4_protocol:ip_forward(
            ProtocolPid,
            Route,
            Packet,
            Ipv4Header
        )
    ),
    ?assertEqual(
        nsime_ipv4_protocol:ip_forward(
            ProtocolPid,
            Route,
            Packet,
            Ipv4Header#nsime_ipv4_header{
                destination_address = nsime_ipv4_address:get_broadcast()
            }
        ),
        ok
    ),
    receive
        {drop_route_error, Ref1} ->
            ok
    end,
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_send_with_header(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),

    Ipv4Header = #nsime_ipv4_header{
        source_address = {10, 107, 1, 1},
        destination_address = {192, 168, 1, 250},
        ttl = 1
    },
    Packet = #nsime_packet{data = <<0:32>>, size = 4},
    Ref1 = make_ref(),
    DropCallback = {
        ?MODULE,
        drop_trace_tester,
        [self(), Ref1]
    },
    ?assertEqual(nsime_ipv4_protocol:set_drop_trace(ProtocolPid, DropCallback), ok),
    ?assertEqual(
        nsime_ipv4_protocol:send_with_header(
            ProtocolPid,
            Packet,
            Ipv4Header,
            undefined
        ),
        ok
    ),
    receive
        {drop_route_error, Ref1} ->
            ok
    end,
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_recv(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    InterfacePid1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfacePid1), ok),

    HL = 5,
    TOS = 200,
    TotalLength = 60,
    Id = 12345,
    Flags = 5,
    FragmentOffset = 5000,
    TTL = 32,
    Protocol = 100,
    SrcAddress = {10, 107, 1, 1},
    DestAddress = {192, 168, 0, 1},
    Ipv4Header = #nsime_ipv4_header{
        header_length = HL,
        tos = TOS,
        total_length = TotalLength,
        identification = Id,
        flags = Flags,
        fragment_offset = FragmentOffset,
        ttl = TTL,
        protocol = Protocol,
        checksum = 0,
        source_address = SrcAddress,
        destination_address = DestAddress,
        calculate_checksum = false,
        checksum_correct = false
    },
    Ipv4HeaderBinary = nsime_ipv4_header:serialize(Ipv4Header),
    Data = <<0:160>>,
    Packet = #nsime_packet{
        data = <<Ipv4HeaderBinary/binary, Data/binary>>,
        size = byte_size(Ipv4HeaderBinary) + byte_size(Data)
    },
    Ref1 = make_ref(),
    DropCallback = {
        ?MODULE,
        drop_trace_tester,
        [self(), Ref1]
    },
    ?assertEqual(nsime_ipv4_protocol:set_drop_trace(ProtocolPid, DropCallback), ok),
    ?assertEqual(
        nsime_ipv4_protocol:recv(
            ProtocolPid,
            DevicePid1,
            Packet,
            ?IPv4_PROTOCOL_NUMBER,
            Ipv4Header#nsime_ipv4_header.source_address,
            Ipv4Header#nsime_ipv4_header.destination_address,
            undefined
        ),
        ok
    ),
    receive
        {drop_route_error, Ref1} ->
            ok
    end,

    nsime_config:start(),
    ?assert(lists:member(nsime_config, erlang:registered())),
    ?assertEqual(nsime_config:disable_checksum(), ok),
    ?assertEqual(nsime_ipv4_protocol:set_up(ProtocolPid, InterfacePid1), ok),
    Ref2 = make_ref(),
    ReceiveCallback = {
        ?MODULE,
        receive_trace_tester,
        [self(), Ref2]
    },
    ?assertEqual(nsime_ipv4_protocol:set_receive_trace(ProtocolPid, ReceiveCallback), ok),
    ?assertEqual(
        nsime_ipv4_protocol:recv(
            ProtocolPid,
            DevicePid1,
            Packet,
            ?IPv4_PROTOCOL_NUMBER,
            Ipv4Header#nsime_ipv4_header.source_address,
            Ipv4Header#nsime_ipv4_header.destination_address,
            undefined
        ),
        ok
    ),
    receive
        {receive_trace, Ref2} ->
            ok
    end,
    receive
        {drop_route_error, Ref1} ->
            ok
    end,

    ?assertEqual(nsime_config:enable_checksum(), ok),
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assertEqual(nsime_ipv4_protocol:set_routing_protocol(ProtocolPid, RoutingPid), ok),
    ?assertEqual(nsime_ipv4_static_routing:set_ipv4_protocol(RoutingPid, ProtocolPid), ok),
    Ipv4Header1 = Ipv4Header#nsime_ipv4_header{calculate_checksum = true},
    Ipv4HeaderBinary1 = nsime_ipv4_header:serialize(Ipv4Header1),
    Packet1 = Packet#nsime_packet{data = <<Ipv4HeaderBinary1/binary, Data/binary>>},
    ?assertEqual(
        nsime_ipv4_protocol:recv(
            ProtocolPid,
            DevicePid1,
            Packet1,
            ?IPv4_PROTOCOL_NUMBER,
            Ipv4Header1#nsime_ipv4_header.source_address,
            Ipv4Header1#nsime_ipv4_header.destination_address,
            undefined
        ),
        ok
    ),
    receive
        {receive_trace, Ref2} ->
            ok
    end,
    receive
        {drop_route_error, Ref1} ->
            ok
    end,
    receive
        {drop_route_error, Ref1} ->
            ok
    end,

    ?assertEqual(nsime_ipv4_interface:set_forwarding(InterfacePid1, true), ok),
    ?assertEqual(
        nsime_ipv4_static_routing:add_host_route(
            RoutingPid,
            DestAddress,
            InterfacePid1,
            0
        ),
        ok
    ),
    Mask = {255, 255, 255, 0},
    AddressPid1 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_pid(AddressPid1)),
    AddressPid2 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid1), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid2), ok),
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assertEqual(
        nsime_ipv4_protocol:recv(
            ProtocolPid,
            DevicePid1,
            Packet1,
            ?IPv4_PROTOCOL_NUMBER,
            Ipv4Header1#nsime_ipv4_header.source_address,
            Ipv4Header1#nsime_ipv4_header.destination_address,
            undefined
        ),
        ok
    ),
    receive
        {receive_trace, Ref2} ->
            ok
    end,

    ?assertEqual(nsime_config:stop(), stopped),
    ?assertEqual(nsime_simulator:stop(), stopped),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_send(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    InterfacePid1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfacePid1), ok),
    nsime_config:start(),
    ?assert(lists:member(nsime_config, erlang:registered())),

    SrcAddress = {10, 107, 1, 1},
    DestAddress = {192, 168, 0, 1},
    Data = <<0:160>>,
    Packet = #nsime_packet{
        data = <<Data/binary>>,
        size = byte_size(Data)
    },
    Mask = {255, 255, 255, 0},
    AddressPid1 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_pid(AddressPid1)),
    AddressPid2 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid1), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid2), ok),

    ?assertError(
        nsime_routing_protocol_undefined,
        nsime_ipv4_protocol:send(
            ProtocolPid,
            Packet,
            SrcAddress,
            DestAddress,
            ?UDP_PROTOCOL_NUMBER,
            undefined
        )
    ),

    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assertEqual(nsime_ipv4_protocol:set_routing_protocol(ProtocolPid, RoutingPid), ok),
    ?assertEqual(nsime_ipv4_static_routing:set_ipv4_protocol(RoutingPid, ProtocolPid), ok),

    Ref1 = make_ref(),
    DropCallback = {
        ?MODULE,
        drop_trace_tester,
        [self(), Ref1]
    },
    ?assertEqual(nsime_ipv4_protocol:set_drop_trace(ProtocolPid, DropCallback), ok),
    ?assertEqual(
        nsime_ipv4_protocol:send(
            ProtocolPid,
            Packet,
            SrcAddress,
            DestAddress,
            ?UDP_PROTOCOL_NUMBER,
            undefined
        ),
        ok
    ),
    receive
        {drop_route_error, Ref1} ->
            ok
    end,

    BroadcastAddress = nsime_ipv4_address:get_broadcast(),
    Ref2 = make_ref(),
    OutgoingCallback = {
        ?MODULE,
        send_outgoing_tester,
        [self(), Ref2]
    },
    ?assertEqual(nsime_ipv4_protocol:set_send_outgoing_trace(ProtocolPid, OutgoingCallback), ok),
    Ref3 = make_ref(),
    TransmitCallback = {
        ?MODULE,
        transmit_trace_tester,
        [self(), Ref3]
    },
    ?assertEqual(nsime_ipv4_protocol:set_transmit_trace(ProtocolPid, TransmitCallback), ok),
    ?assertEqual(
        nsime_ipv4_protocol:send(
            ProtocolPid,
            Packet,
            SrcAddress,
            BroadcastAddress,
            ?UDP_PROTOCOL_NUMBER,
            undefined
        ),
        ok
    ),
    receive
        {send_outgoing_trace, Ref2} ->
            ok
    end,
    receive
        {transmit_trace, Ref3} ->
            ok
    end,

    Route = #nsime_ipv4_route{
        output_device = DevicePid1,
        gateway = nsime_ipv4_address:get_zero()
    },
    ?assertError(
        nsime_situation_not_implemented,
        nsime_ipv4_protocol:send(
            ProtocolPid,
            Packet,
            SrcAddress,
            DestAddress,
            ?UDP_PROTOCOL_NUMBER,
            Route
        )
    ),

    NewRoute = Route#nsime_ipv4_route{
        gateway = DestAddress
    },
    ?assertEqual(
        nsime_ipv4_protocol:send(
            ProtocolPid,
            Packet,
            SrcAddress,
            DestAddress,
            ?UDP_PROTOCOL_NUMBER,
            NewRoute
        ),
        ok
    ),
    receive
        {send_outgoing_trace, Ref2} ->
            ok
    end,
    SubnetBroadcastAddress = nsime_ipv4_address:get_subnet_directed_broadcast(
        DestAddress,
        Mask
    ),
    ?assertEqual(
        nsime_ipv4_protocol:send(
            ProtocolPid,
            Packet,
            SrcAddress,
            SubnetBroadcastAddress,
            ?UDP_PROTOCOL_NUMBER,
            Route
        ),
        ok
    ),
    receive
        {send_outgoing_trace, Ref2} ->
            ok
    end,
    receive
        {transmit_trace, Ref3} ->
            ok
    end,
    ?assertEqual(
        nsime_ipv4_static_routing:add_network_route(
            RoutingPid,
            DestAddress,
            Mask,
            InterfacePid1,
            0
        ),
        ok
    ),
    ?assertEqual(
        nsime_ipv4_protocol:send(
            ProtocolPid,
            Packet,
            SrcAddress,
            DestAddress,
            ?UDP_PROTOCOL_NUMBER,
            undefined
        ),
        ok
    ),
    receive
        {send_outgoing_trace, Ref2} ->
            ok
    end,
    receive
        {drop_route_error, Ref1} ->
            ok
    end,


    ?assertEqual(nsime_config:stop(), stopped),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_cast_info_codechange(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    gen_server:cast(ProtocolPid, junk),
    ProtocolPid ! junk,
    nsime_ipv4_protocol:code_change(junk, junk, junk),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

%% Helper methods %%

drop_trace_tester(From, Ref, _Ipv4Header, _Packet, _ErrorType, _Self, _None) ->
    spawn(fun() -> From ! {drop_route_error, Ref} end).

unicast_forward_tester(From, Ref, _Ipv4Header, _Packet, _InterfacePid) ->
    spawn(fun() -> From ! {unicast_forward_trace, Ref} end).

send_outgoing_tester(From, Ref, _Ipv4Header, _Packet, _InterfacePid) ->
    spawn(fun() -> From ! {send_outgoing_trace, Ref} end).

transmit_trace_tester(From, Ref, _Ipv4Header, _Packet, _InterfacePid) ->
    spawn(fun() -> From ! {transmit_trace, Ref} end).

receive_trace_tester(From, Ref, _Ipv4Header, _Packet, _InterfacePid) ->
    spawn(fun() -> From ! {receive_trace, Ref} end).
