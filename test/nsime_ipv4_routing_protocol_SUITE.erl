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

%% Purpose : Test module for nsime_ipv4_routing_protocol
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_routing_protocol_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_ipv4_route.hrl").
-include("nsime_ipv4_routing_table_entry.hrl").
-include("nsime_ipv4_static_routing_state.hrl").

all() -> [
            test_creation_shutdown,
            test_notify_interfaces_updown,
            test_add_remove_addresses,
            test_route_input,
            test_route_output,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assert(is_pid(RoutingPid)),
    Ipv4ProtocolPid = nsime_ipv4_protocol:create(),
    ?assertEqual(nsime_ipv4_routing_protocol:set_ipv4_protocol(RoutingPid, Ipv4ProtocolPid), ok),
    ?assertEqual(nsime_ipv4_protocol:destroy(Ipv4ProtocolPid), stopped),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped).

test_notify_interfaces_updown(_) ->
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assert(is_pid(RoutingPid)),
    InterfacePid1 = nsime_ipv4_interface:create(),
    Address1 = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressPid1 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_pid(AddressPid1)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid1), ok),
    AddressPid2 = nsime_ipv4_interface_address:create(),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid2), ok),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingPid), []),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_interface_up(
            RoutingPid,
            InterfacePid1
        ),
        ok
    ),
    InterfacePid2 = nsime_ipv4_interface:create(),
    Address2 = {10, 107, 2, 1},
    AddressPid3 = nsime_ipv4_interface_address:create(Address2, Mask),
    ?assert(is_pid(AddressPid3)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid2, AddressPid3), ok),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_interface_up(
            RoutingPid,
            InterfacePid2
        ),
        ok
    ),
    NetworkRoute1 = nsime_ipv4_routing_table_entry:create_network_route(
        nsime_ipv4_address:combine_mask(Address1, Mask),
        Mask,
        InterfacePid1,
        0
    ),
    NetworkRoute2 = nsime_ipv4_routing_table_entry:create_network_route(
        nsime_ipv4_address:combine_mask(Address2, Mask),
        Mask,
        InterfacePid2,
        0
    ),
    ?assert(
        lists:member(
            NetworkRoute1,
            nsime_ipv4_static_routing:get_network_routes(RoutingPid)
        )
    ),
    ?assert(
        lists:member(
            NetworkRoute2,
            nsime_ipv4_static_routing:get_network_routes(RoutingPid)
        )
    ),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_interface_down(
            RoutingPid,
            InterfacePid1
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingPid), [NetworkRoute2]),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_interface_down(
            RoutingPid,
            InterfacePid2
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingPid), []),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped).

test_add_remove_addresses(_) ->
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assert(is_pid(RoutingPid)),
    InterfacePid = nsime_ipv4_interface:create(),
    AddressPid = nsime_ipv4_interface_address:create(),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid, AddressPid), ok),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_add_address(
            RoutingPid,
            InterfacePid,
            AddressPid
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_interface:set_up(InterfacePid), ok),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_add_address(
            RoutingPid,
            InterfacePid,
            AddressPid
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingPid), []),
    Address = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    ?assertEqual(
        nsime_ipv4_interface_address:set_local_address(AddressPid, Address),
        ok
    ),
    ?assertEqual(nsime_ipv4_interface_address:set_mask(AddressPid, Mask), ok),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_add_address(
            RoutingPid,
            InterfacePid,
            AddressPid
        ),
        ok
    ),
    NetworkRoute = nsime_ipv4_routing_table_entry:create_network_route(
        nsime_ipv4_address:combine_mask(Address, Mask),
        Mask,
        InterfacePid,
        0
    ),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingPid), [NetworkRoute]),
    ?assertEqual(nsime_ipv4_interface:set_down(InterfacePid), ok),
    ?assertEqual(
        nsime_ipv4_static_routing:add_host_route(
            RoutingPid,
            Address,
            InterfacePid,
            0
        ),
        ok
    ),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_remove_address(
            RoutingPid,
            InterfacePid,
            AddressPid
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_interface:set_up(InterfacePid), ok),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_remove_address(
            RoutingPid,
            InterfacePid,
            AddressPid
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped).

test_route_input(_) ->
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assert(is_pid(RoutingPid)),
    InterfacePid1 = nsime_ipv4_interface:create(),
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    ?assertEqual(nsime_ptp_netdevice:set_interface(DevicePid, InterfacePid1), ok),
    ?assertEqual(nsime_ipv4_interface:set_device(InterfacePid1, DevicePid), ok),
    DestinationAddress1 = {224, 0, 0, 1},
    Ipv4Header1 = #nsime_ipv4_header{
        destination_address = DestinationAddress1
    },
    Packet = #nsime_packet{},
    ?assertEqual(
        catch nsime_ipv4_routing_protocol:route_input(
            RoutingPid,
            Packet,
            Ipv4Header1,
            DevicePid,
            junk,
            junk,
            junk,
            junk,
            []
        ),
        options_not_supported
    ),

    DestinationAddress2 = {10, 107, 1, 1},
    Mask1 = {255, 255, 255, 0},
    AddressPid1 = nsime_ipv4_interface_address:create(DestinationAddress2, Mask1),
    ?assert(is_pid(AddressPid1)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid1), ok),
    Ipv4Header2 = #nsime_ipv4_header{
        destination_address = DestinationAddress2
    },
    ?assertEqual(
        nsime_ipv4_routing_protocol:route_input(
            RoutingPid,
            Packet,
            Ipv4Header2,
            DevicePid,
            junk,
            junk,
            junk,
            {erlang, date, []},
            []
        ),
        false
    ),
    ?assertEqual(
        nsime_ipv4_routing_protocol:route_input(
            RoutingPid,
            Packet,
            Ipv4Header2,
            DevicePid,
            junk,
            junk,
            undefined,
            undefined,
            [InterfacePid1]
        ),
        false
    ),

    Ref1 = make_ref(),
    ErrorCallback = {
        ?MODULE,
        error_callback_tester,
        [self(), Ref1]
    },
    Ref2 = make_ref(),
    LocalDeliverCallback = {
        ?MODULE,
        local_deliver_tester,
        [self(), Ref2]
    },
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assertEqual(
        nsime_ipv4_routing_protocol:route_input(
            RoutingPid,
            Packet,
            Ipv4Header2,
            DevicePid,
            junk,
            junk,
            LocalDeliverCallback,
            ErrorCallback,
            [InterfacePid1]
        ),
        true
    ),
    nsime_simulator:run(),
    receive
        {local_deliver, Ref2} ->
            ok
    end,
    ?assertEqual(nsime_ipv4_interface:set_forwarding(InterfacePid1, false), ok),
    ?assertEqual(
        nsime_ipv4_routing_protocol:route_input(
            RoutingPid,
            Packet,
            Ipv4Header2,
            DevicePid,
            junk,
            junk,
            LocalDeliverCallback,
            ErrorCallback,
            []
        ),
        false
    ),
    nsime_simulator:run(),
    receive
        {error_callback, Ref1} ->
            ok
    end,

    ?assertEqual(nsime_ipv4_interface:set_forwarding(InterfacePid1, true), ok),
    Mask2 = {255, 255, 0, 0},
    AddressPid2 = nsime_ipv4_interface_address:create(DestinationAddress2, Mask2),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface:set_up(InterfacePid1), ok),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_add_address(
            RoutingPid,
            InterfacePid1,
            AddressPid2
        ),
        ok
    ),

    Ref3 = make_ref(),
    UnicastForwardCallback = {
        ?MODULE,
        unicast_forward_tester,
        [self(), Ref3]
    },
    ?assertEqual(
        nsime_ipv4_routing_protocol:route_input(
            RoutingPid,
            Packet,
            Ipv4Header2,
            DevicePid,
            UnicastForwardCallback,
            junk,
            LocalDeliverCallback,
            ErrorCallback,
            []
        ),
        true
    ),
    nsime_simulator:run(),
    receive
        {unicast_forward, Ref3} ->
            ok
    end,

    ?assertEqual(nsime_simulator:stop(), stopped),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped).

test_route_output(_) ->
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assert(is_pid(RoutingPid)),
    InterfacePid1 = nsime_ipv4_interface:create(),
    Address1 = {10, 107, 1, 1},
    Mask1 = {255, 255, 255, 0},
    AddressPid1 = nsime_ipv4_interface_address:create(Address1, Mask1),
    ?assert(is_pid(AddressPid1)),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid1, AddressPid1), ok),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_interface_up(
            RoutingPid,
            InterfacePid1
        ),
        ok
    ),
    DestinationAddress = {224, 0, 0, 1},
    Ipv4Header = #nsime_ipv4_header{
        destination_address = DestinationAddress
    },
    Packet = #nsime_packet{},
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    ?assertEqual(nsime_ptp_netdevice:set_interface(DevicePid, InterfacePid1), ok),
    ?assertEqual(nsime_ipv4_interface:set_device(InterfacePid1, DevicePid), ok),
    Route = #nsime_ipv4_route{
        destination = DestinationAddress,
        source = Address1,
        gateway = nsime_ipv4_address:get_zero(),
        output_device = DevicePid
    },
    ?assertMatch(
        {error_noterror, Route},
        nsime_ipv4_routing_protocol:route_output(
            RoutingPid,
            Packet,
            Ipv4Header,
            DevicePid
        )
    ),
    DestinationAddress1 = {192, 168, 1, 1},
    ?assertMatch(
        {error_noroutetohost, undefined},
        nsime_ipv4_routing_protocol:route_output(
            RoutingPid,
            Packet,
            Ipv4Header#nsime_ipv4_header{
                destination_address = DestinationAddress1
            },
            DevicePid
        )
    ),
    ?assertMatch(
        {error_noroutetohost, undefined},
        nsime_ipv4_routing_protocol:route_output(
            RoutingPid,
            Packet,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            list_to_pid("<0.0.0>")
        )
    ),
    Mask2 = {255, 255, 0, 0},
    AddressPid2 = nsime_ipv4_interface_address:create(Address1, Mask2),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(nsime_ipv4_interface:set_up(InterfacePid1), ok),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_add_address(
            RoutingPid,
            InterfacePid1,
            AddressPid2
        ),
        ok
    ),
    Route2 = #nsime_ipv4_route{
        destination = nsime_ipv4_address:combine_mask(
            Address1,
            Mask1
        ),
        source = Address1,
        gateway = nsime_ipv4_address:get_zero(),
        output_device = DevicePid
    },
    ?assertMatch(
        {error_noterror, Route2},
        nsime_ipv4_routing_protocol:route_output(
            RoutingPid,
            Packet,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid
        )
    ),
    ?assertEqual(nsime_ipv4_interface_address:set_secondary(AddressPid1), ok),
    Route3 = #nsime_ipv4_route{
        destination = nsime_ipv4_address:combine_mask(
            Address1,
            Mask1
        ),
        source = Address1,
        gateway = nsime_ipv4_address:get_zero(),
        output_device = DevicePid
    },
    ?assertMatch(
        {error_noterror, Route3},
        nsime_ipv4_routing_protocol:route_output(
            RoutingPid,
            Packet,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid
        )
    ),

    Mask3 = {255, 255, 255, 128},
    AddressPid3 = nsime_ipv4_interface_address:create(Address1, Mask3),
    ?assert(is_pid(AddressPid3)),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_add_address(
            RoutingPid,
            InterfacePid1,
            AddressPid3
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_interface_address:set_primary(AddressPid1), ok),
    ?assertMatch(
        {error_noterror, Route3},
        nsime_ipv4_routing_protocol:route_output(
            RoutingPid,
            Packet,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid
        )
    ),
    AddressPid4 = nsime_ipv4_interface_address:create(Address1, Mask3),
    ?assert(is_pid(AddressPid4)),
    ?assertEqual(
        nsime_ipv4_routing_protocol:notify_add_address(
            RoutingPid,
            InterfacePid1,
            AddressPid4
        ),
        ok
    ),
    ?assertMatch(
        {error_noterror, Route3},
        nsime_ipv4_routing_protocol:route_output(
            RoutingPid,
            Packet,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid
        )
    ),

    Gateway1 = {10, 250, 1, 1},
    ?assertEqual(
        nsime_ipv4_static_routing:add_host_route(
            RoutingPid,
            Address1,
            Gateway1,
            InterfacePid1,
            50
        ),
        ok
    ),
    Gateway2 = {10, 250, 1, 2},
    ?assertEqual(
        nsime_ipv4_static_routing:add_host_route(
            RoutingPid,
            Address1,
            Gateway2,
            InterfacePid1,
            10
        ),
        ok
    ),

    Route4 = #nsime_ipv4_route{
        destination = Address1,
        source = Address1,
        gateway = Gateway2,
        output_device = DevicePid
    },
    ?assertMatch(
        {error_noterror, Route4},
        nsime_ipv4_routing_protocol:route_output(
            RoutingPid,
            Packet,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid
        )
    ),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped).

test_cast_info_codechange(_) ->
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assert(is_pid(RoutingPid)),
    gen_server:cast(RoutingPid, junk),
    RoutingPid ! junk,
    nsime_ipv4_static_routing:code_change(junk, junk, junk),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped).

%% Helper methods %%

error_callback_tester(From, Ref, _Packet, _Ipv4Header, _Type, _From, _Route) ->
    spawn(fun() -> From ! {error_callback, Ref} end).

local_deliver_tester(From, Ref, _Packet, _Header, _Interface) ->
    spawn(fun() -> From ! {local_deliver, Ref} end).

unicast_forward_tester(From, Ref, _Route, _Packet, _Header) ->
    spawn(fun() -> From ! {unicast_forward, Ref} end).