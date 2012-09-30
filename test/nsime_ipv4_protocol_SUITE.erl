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
-include("nsime_ipv4_protocol_state.hrl").
-include("nsime_icmpv4_protocol_state.hrl").
-include("nsime_udp_protocol_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
            test_route_input_error,
            test_is_destination,
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
