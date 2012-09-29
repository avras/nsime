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
-include("nsime_ipv4_protocol_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
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
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assertEqual(nsime_ipv4_protocol:set_routing_protocol(ProtocolPid, RoutingPid), ok),
    ?assertEqual(nsime_ipv4_protocol:get_routing_protocol(ProtocolPid), RoutingPid),
    ?assertEqual(nsime_ipv4_protocol:create_raw_socket(ProtocolPid), ok),
    ?assertEqual(nsime_ipv4_protocol:delete_raw_socket(ProtocolPid, undefined), ok),
    Layer4ProtPid = list_to_pid("<0.0.0>"),
    ?assertEqual(nsime_ipv4_protocol:insert_layer4_protocol(ProtocolPid, Layer4ProtPid), ok),
    ?assertEqual(nsime_ipv4_protocol:remove_layer4_protocol(ProtocolPid, Layer4ProtPid), ok),
    ?assertEqual(nsime_ipv4_protocol:set_default_ttl(ProtocolPid, 64), ok),
    DevicePid = nsime_ptp_netdevice:create(),
    InterfacePid = nsime_ipv4_protocol:add_interface(ProtocolPid, DevicePid),
    ?assert(is_pid(InterfacePid)),
    ?assertEqual(nsime_ipv4_protocol:get_interface_list(ProtocolPid), [InterfacePid]),
    Address = {10, 107, 1, 1},
    AddressPid1 = nsime_ipv4_interface_address:create(),
    ?assert(is_pid(AddressPid1)),
    ?assertEqual(nsime_ipv4_interface_address:set_local_address(AddressPid1, Address), ok),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid, AddressPid1), ok),
    ?assertEqual(nsime_ipv4_protocol:get_interface_for_address(ProtocolPid, Address), InterfacePid),
    Address2 = {10, 107, 10, 20},
    Mask = {255, 255, 0, 0},
    ?assertEqual(nsime_ipv4_protocol:get_interface_for_prefix(ProtocolPid, Address2, Mask), InterfacePid),
    ?assertEqual(nsime_ipv4_protocol:get_interface_for_device(ProtocolPid, DevicePid), InterfacePid),
    ?assertEqual(nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid), [AddressPid1]),
    AddressPid2 = nsime_ipv4_interface_address:create(),
    ?assertEqual(nsime_ipv4_protocol:add_interface_address(ProtocolPid, InterfacePid, AddressPid2), ok),
    ?assert(
        lists:member(
            AddressPid1,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid)
        )
     ),
    ?assert(
        lists:member(
            AddressPid2,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid)
        )
     ),
    ?assertEqual(nsime_ipv4_protocol:remove_interface_address(ProtocolPid, InterfacePid, AddressPid2), ok),
    ?assert(
        lists:member(
            AddressPid1,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid)
        )
     ),
    ?assertNot(
        lists:member(
            AddressPid2,
            nsime_ipv4_protocol:get_interface_address_list(ProtocolPid, InterfacePid)
        )
     ),

    ?assertEqual(nsime_ipv4_protocol:select_source_address(ProtocolPid, undefined, undefined, undefined), ok),

    Metric = 43,
    ?assertEqual(nsime_ipv4_protocol:set_metric(ProtocolPid, InterfacePid, Metric), ok),
    ?assertEqual(nsime_ipv4_protocol:get_metric(ProtocolPid, InterfacePid), Metric),

    ?assertEqual(
        nsime_ipv4_protocol:get_mtu(ProtocolPid, InterfacePid),
        nsime_ptp_netdevice:get_mtu(DevicePid)
    ),

    ?assertNot(nsime_ipv4_protocol:is_up(ProtocolPid, InterfacePid)),
    ?assertEqual(nsime_ipv4_protocol:set_up(ProtocolPid, InterfacePid), ok),
    ?assert(nsime_ipv4_protocol:is_up(ProtocolPid, InterfacePid)),
    ?assertEqual(nsime_ipv4_protocol:set_down(ProtocolPid, InterfacePid), ok),
    ?assertNot(nsime_ipv4_protocol:is_up(ProtocolPid, InterfacePid)),

    ?assertEqual(nsime_ipv4_protocol:set_forwarding(ProtocolPid, InterfacePid, false), ok),
    ?assertNot(nsime_ipv4_protocol:is_forwarding(ProtocolPid, InterfacePid)),
    ?assertEqual(nsime_ipv4_protocol:set_forwarding(ProtocolPid, InterfacePid, true), ok),
    ?assert(nsime_ipv4_protocol:is_forwarding(ProtocolPid, InterfacePid)),

    ?assertEqual(nsime_ipv4_protocol:get_netdevice(ProtocolPid, InterfacePid), DevicePid),
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

    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid), stopped),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).

test_cast_info_codechange(_) ->
    ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    gen_server:cast(ProtocolPid, junk),
    ProtocolPid ! junk,
    nsime_ipv4_protocol:code_change(junk, junk, junk),
    ?assertEqual(nsime_ipv4_protocol:destroy(ProtocolPid), stopped).
