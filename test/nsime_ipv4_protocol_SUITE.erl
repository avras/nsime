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
-include("nsime_ipv4_routing_table_entry.hrl").
-include("nsime_ipv4_static_routing_state.hrl").
-include("nsime_ipv4_list_routing_state.hrl").
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").
-include("nsime_ipv4_protocol_state.hrl").
-include("nsime_icmpv4_protocol_state.hrl").
-include("nsime_ip_endpoint_state.hrl").
-include("nsime_ip_endpoint_demux_state.hrl").
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
    nsime_config:start(),
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped),
    ?assertEqual(nsime_config:stop(), stopped).

test_set_get_components(_) ->
    nsime_simulator:start(),
    nsime_config:start(),
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    ?assertEqual(nsime_ipv4_protocol:protocol_number(), ?IPv4_PROTOCOL_NUMBER),
    ?assertEqual(gen_server:call(ProtocolPid, protocol_number), ?IPv4_PROTOCOL_NUMBER),
    DevicePid1 = nsime_ptp_netdevice:create(),
    InterfaceState1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    InterfaceId1 = nsime_ipv4_interface:get_id(InterfaceState1),
    ?assert(is_record(InterfaceState1, nsime_ipv4_interface_state)),
    ?assertEqual(nsime_ipv4_protocol:set_up(ProtocolPid, InterfaceId1), ok),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfaceId1), ok),
    Routing = nsime_ipv4_static_routing:create(),
    ?assertEqual(nsime_ipv4_protocol:set_routing_protocol(ProtocolPid, Routing), ok),
    ?assertEqual(nsime_ipv4_protocol:get_routing_protocol(ProtocolPid), Routing),
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
    ?assertEqual(nsime_ipv4_protocol:get_interface_list(ProtocolPid), [InterfaceState1]),
    Address1 = {10, 107, 1, 1},
    AddressState1 = nsime_ipv4_interface_address:create(),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    AddressState2 = nsime_ipv4_interface_address:set_local_address(AddressState1, Address1),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState2), ok),
    DevicePid2 = nsime_ptp_netdevice:create(),
    InterfaceState2 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    InterfaceId2 = nsime_ipv4_interface:get_id(InterfaceState2),
    ?assert(is_record(InterfaceState2, nsime_ipv4_interface_state)),
    ?assertEqual(
        nsime_ipv4_interface:get_id(nsime_ipv4_protocol:get_interface_for_address(ProtocolPid, Address1)),
        InterfaceId1
    ),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId2, AddressState2), ok),
    ?assert(
        (nsime_ipv4_interface:get_id(nsime_ipv4_protocol:get_interface_for_address(ProtocolPid, Address1)) == InterfaceId1)
        or
        (nsime_ipv4_interface:get_id(nsime_ipv4_protocol:get_interface_for_address(ProtocolPid, Address1)) == InterfaceId2)
    ),
    ?assertEqual(
        nsime_ipv4_interface:get_id(nsime_ipv4_protocol:get_interface_for_device(ProtocolPid, DevicePid1)),
        InterfaceId1
    ),
    ?assertEqual(nsime_ipv4_protocol:remove_interface_address(ProtocolPid, InterfaceId2, AddressState2), ok),
    Address2 = {10, 107, 10, 20},
    Mask = {255, 255, 0, 0},
    Address3 = {10, 107, 1, 1},
    AddressState3 = nsime_ipv4_interface_address:create(),
    ?assert(is_record(AddressState3, nsime_ipv4_interface_address_state)),
    AddressState4 = nsime_ipv4_interface_address:set_local_address(AddressState3, Address3),
    InterfaceState3 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    InterfaceId3 = nsime_ipv4_interface:get_id(InterfaceState3),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId3, AddressState4), ok),
    ?assert(
        (nsime_ipv4_interface:get_id(nsime_ipv4_protocol:get_interface_for_prefix(ProtocolPid, Address2, Mask)) == InterfaceId1)
        or
        (nsime_ipv4_interface:get_id(nsime_ipv4_protocol:get_interface_for_prefix(ProtocolPid, Address2, Mask)) == InterfaceId3)
    ),
    ?assert(
        (nsime_ipv4_interface:get_id(nsime_ipv4_protocol:get_interface_for_device(ProtocolPid, DevicePid2)) == InterfaceId2)
        or
        (nsime_ipv4_interface:get_id(nsime_ipv4_protocol:get_interface_for_device(ProtocolPid, DevicePid2)) == InterfaceId3)
    ),
    ?assertEqual(nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfaceId1), [AddressState2]),
    AddressState5 = nsime_ipv4_interface_address:create(),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState5), ok),
    ?assert(
        lists:member(
            AddressState2,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfaceId1)
        )
     ),
    ?assert(
        lists:member(
            AddressState5,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfaceId1)
        )
     ),
    ?assertEqual(nsime_ipv4_protocol:remove_interface_address(ProtocolPid, InterfaceId1, AddressState5), ok),
    ?assert(
        lists:member(
            AddressState2,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfaceId1)
        )
     ),
    ?assertNot(
        lists:member(
            AddressState5,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfaceId1)
        )
     ),

    ?assertEqual(nsime_ipv4_protocol:select_source_address(ProtocolPid, undefined, undefined, undefined), ok),

    Metric = 43,
    ?assertEqual(nsime_ipv4_protocol:set_metric(ProtocolPid, InterfaceId1, Metric), ok),
    ?assertEqual(nsime_ipv4_protocol:get_metric(ProtocolPid, InterfaceId1), Metric),

    ?assertEqual(
        nsime_ipv4_protocol:get_mtu(ProtocolPid, InterfaceId1),
        nsime_ptp_netdevice:get_mtu(DevicePid1)
    ),

    ?assertNot(nsime_ipv4_protocol:is_up(ProtocolPid, InterfaceId1)),
    ?assertEqual(nsime_ipv4_protocol:set_up(ProtocolPid, InterfaceId1), ok),
    ?assert(nsime_ipv4_protocol:is_up(ProtocolPid, InterfaceId1)),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfaceId1), ok),
    ?assertNot(nsime_ipv4_protocol:is_up(ProtocolPid, InterfaceId1)),

    ?assertEqual(nsime_ipv4_protocol:set_forwarding(ProtocolPid, InterfaceId1, false), ok),
    ?assertNot(nsime_ipv4_protocol:is_forwarding(ProtocolPid, InterfaceId1)),
    ?assertEqual(nsime_ipv4_protocol:set_forwarding(ProtocolPid, InterfaceId1, true), ok),
    ?assert(nsime_ipv4_protocol:is_forwarding(ProtocolPid, InterfaceId1)),

    ?assertEqual(nsime_ipv4_protocol:get_netdevice(ProtocolPid, InterfaceId1), DevicePid1),
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

    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid1), stopped),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid2), stopped),
    ?assertEqual(nsime_udp_protocol:destroy(Layer4ProtPid1), stopped),
    ?assertEqual(nsime_udp_protocol:destroy(Layer4ProtPid2), stopped),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_route_input_error(_) ->
    nsime_config:start(),
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
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped),
    ?assertEqual(nsime_config:stop(), stopped).

test_is_destination(_) ->
    nsime_simulator:start(),
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    InterfaceState1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    InterfaceId1 = nsime_ipv4_interface:get_id(InterfaceState1),
    Address1 = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    AddressState2 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState1), ok),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState2), ok),
    InterfaceList1 = nsime_ipv4_protocol:get_interface_list(ProtocolPid),
    [NewInterfaceState1] = lists:filter(fun(I) -> nsime_ipv4_interface:get_id(I) == InterfaceId1 end, InterfaceList1),
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address1, NewInterfaceState1)),
    Address2 = nsime_ipv4_address:get_subnet_directed_broadcast(Address1, Mask),
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address2, NewInterfaceState1)),
    Address3 = nsime_ipv4_address:get_broadcast(),
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address3, NewInterfaceState1)),
    Address4 = {225, 107, 1, 1},
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address4, NewInterfaceState1)),
    Address5 = {10, 250, 1, 1},
    ?assertEqual(nsime_ipv4_protocol:set_weak_es_model(ProtocolPid, false), ok),
    ?assertNot(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address5, NewInterfaceState1)),
    DevicePid2 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid2)),
    InterfaceState2 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    InterfaceId2 = nsime_ipv4_interface:get_id(InterfaceState2),
    InterfaceState3 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    InterfaceId3 = nsime_ipv4_interface:get_id(InterfaceState3),
    AddressState3 = nsime_ipv4_interface_address:create(Address5, Mask),
    ?assert(is_record(AddressState3, nsime_ipv4_interface_address_state)),
    AddressState4 = nsime_ipv4_interface_address:create(Address5, Mask),
    ?assert(is_record(AddressState4, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId2, AddressState3), ok),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId2, AddressState4), ok),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId3, AddressState3), ok),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId3, AddressState4), ok),
    ?assertEqual(nsime_ipv4_protocol:set_weak_es_model(ProtocolPid, true), ok),
    ?assert(nsime_ipv4_protocol:is_destination_address(ProtocolPid, Address5, NewInterfaceState1)),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_ip_forward_no_ttl_error(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid2 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid2)),
    nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid2),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    InterfaceState1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    InterfaceId1 = nsime_ipv4_interface:get_id(InterfaceState1),
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
    ?assertEqual(nsime_ipv4_protocol:set_up(ProtocolPid, InterfaceId1), ok),
    Ref3 = make_ref(),
    TransmitCallback = {
        ?MODULE,
        transmit_trace_tester,
        [self(), Ref3]
    },
    ?assertEqual(nsime_ipv4_protocol:set_transmit_trace(ProtocolPid, TransmitCallback), ok),
    Address1 = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    AddressState2 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState1), ok),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState2), ok),
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
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfaceId1), ok),
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
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_ip_forward_with_ttl_error(_) ->
    nsime_simulator:start(),
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
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_send_with_header(_) ->
    nsime_simulator:start(),
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
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_recv(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    InterfaceState1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    InterfaceId1 = nsime_ipv4_interface:get_id(InterfaceState1),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfaceId1), ok),

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

    ?assertEqual(nsime_config:disable_checksum(), ok),
    ?assertEqual(nsime_ipv4_protocol:set_up(ProtocolPid, InterfaceId1), ok),
    Ref2 = make_ref(),
    ReceiveCallback = {
        ?MODULE,
        receive_trace_tester,
        [self(), Ref2]
    },
    ?assertEqual(nsime_ipv4_protocol:set_receive_trace(ProtocolPid, ReceiveCallback), ok),
    Routing = nsime_ipv4_static_routing:create(),
    ?assertEqual(nsime_ipv4_protocol:set_routing_protocol(ProtocolPid, Routing), ok),
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

    ?assertEqual(nsime_ipv4_protocol:set_forwarding(ProtocolPid, InterfaceId1, true), ok),
    Mask = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    AddressState2 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState1), ok),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState2), ok),
    Layer4ProtPid1 = nsime_udp_protocol:create(),
    ?assertEqual(nsime_ipv4_protocol:insert_layer4_protocol(ProtocolPid, Layer4ProtPid1), ok),
    Ipv4Header2 = Ipv4Header#nsime_ipv4_header{
        calculate_checksum = true,
        protocol = ?UDP_PROTOCOL_NUMBER
    },
    Ipv4HeaderBinary2 = nsime_ipv4_header:serialize(Ipv4Header2),
    Packet2 = Packet#nsime_packet{data = <<Ipv4HeaderBinary2/binary, Data/binary>>},
    ?assertEqual(
        nsime_ipv4_protocol:recv(
            ProtocolPid,
            DevicePid1,
            Packet2,
            ?IPv4_PROTOCOL_NUMBER,
            Ipv4Header2#nsime_ipv4_header.source_address,
            Ipv4Header2#nsime_ipv4_header.destination_address,
            undefined
        ),
        ok
    ),
    receive
        {receive_trace, Ref2} ->
            ok
    end,

    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_send(_) ->
    nsime_simulator:start(),
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    NodePid = nsime_node:create(),
    ?assertEqual(nsime_ipv4_protocol:set_node(ProtocolPid, NodePid), ok),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    InterfaceState1 = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid1),
    InterfaceId1 = nsime_ipv4_interface:get_id(InterfaceState1),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfaceId1), ok),

    SrcAddress = {10, 107, 1, 1},
    DestAddress = {192, 168, 0, 1},
    Data = <<0:160>>,
    Packet = #nsime_packet{
        data = <<Data/binary>>,
        size = byte_size(Data)
    },
    Mask = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    AddressState2 = nsime_ipv4_interface_address:create(DestAddress, Mask),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState1), ok),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfaceId1, AddressState2), ok),

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

    Routing = nsime_ipv4_static_routing:create(),
    ?assertEqual(nsime_ipv4_protocol:set_routing_protocol(ProtocolPid, Routing), ok),
    InterfaceList = nsime_ipv4_protocol:get_interface_list(ProtocolPid),

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
        output_device = DevicePid1
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

    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_cast_info_codechange(_) ->
    nsime_config:start(),
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    gen_server:cast(ProtocolPid, junk),
    ProtocolPid ! junk,
    nsime_ipv4_protocol:code_change(junk, junk, junk),
    ?assertEqual(nsime_config:stop(), stopped),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

%% Helper methods %%

drop_trace_tester(From, Ref, _Ipv4Header, _Packet, _ErrorType, _Self, _None) ->
    spawn(fun() -> From ! {drop_route_error, Ref} end).

unicast_forward_tester(From, Ref, _Ipv4Header, _Packet, _InterfaceState) ->
    spawn(fun() -> From ! {unicast_forward_trace, Ref} end).

send_outgoing_tester(From, Ref, _Ipv4Header, _Packet, _InterfaceState) ->
    spawn(fun() -> From ! {send_outgoing_trace, Ref} end).

transmit_trace_tester(From, Ref, _Ipv4Header, _Packet, _InterfaceState) ->
    spawn(fun() -> From ! {transmit_trace, Ref} end).

receive_trace_tester(From, Ref, _Ipv4Header, _Packet, _InterfaceState) ->
    spawn(fun() -> From ! {receive_trace, Ref} end).
