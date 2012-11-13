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

%% Purpose : Test module for nsime_ipv4_static_routing
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_static_routing_SUITE).
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
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").

all() -> [
            test_creation_shutdown,
            test_add_routes,
            test_notify_interfaces_updown,
            test_add_remove_addresses,
            test_route_input,
            test_route_output
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    {nsime_ipv4_static_routing, RoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(RoutingState, nsime_ipv4_static_routing_state)),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingState), []).

test_add_routes(_) ->
    {nsime_ipv4_static_routing, RoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(RoutingState, nsime_ipv4_static_routing_state)),
    InterfaceState = nsime_ipv4_interface:create(),
    InterfaceId = nsime_ipv4_interface:get_id(InterfaceState),
    NetworkAddress = {10, 107, 1, 1},
    NetworkMask = {255, 255, 255, 0},
    Metric = 5,
    NetworkRoute1 = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        NetworkMask,
        InterfaceId,
        Metric
    ),
    RoutingState1 = nsime_ipv4_static_routing:add_network_route(
        RoutingState,
        NetworkAddress,
        NetworkMask,
        InterfaceId,
        Metric
    ),
    ?assert(is_record(RoutingState1, nsime_ipv4_static_routing_state)),
    RouteList1 = nsime_ipv4_static_routing:get_network_routes(RoutingState1),
    ?assert(lists:member(NetworkRoute1, RouteList1)),

    NextHop = {10, 250, 1, 1},
    NetworkRoute2 = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        NetworkMask,
        NextHop,
        InterfaceId,
        Metric
    ),
    ?assertNot(lists:member(NetworkRoute2, RouteList1)),
    RoutingState2 = nsime_ipv4_static_routing:add_network_route(
        RoutingState1,
        NetworkAddress,
        NetworkMask,
        NextHop,
        InterfaceId,
        Metric
    ),
    RouteList2 = nsime_ipv4_static_routing:get_network_routes(RoutingState2),
    ?assert(lists:member(NetworkRoute2, RouteList2)),
    ?assert(lists:member(NetworkRoute1, RouteList2)),

    NetworkRoute3 = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        nsime_ipv4_mask:get_ones(),
        InterfaceId,
        Metric
    ),
    ?assertNot(lists:member(NetworkRoute3, RouteList2)),
    ?assertNot(lists:member(NetworkRoute3, RouteList1)),
    RoutingState3 = nsime_ipv4_static_routing:add_host_route(
        RoutingState2,
        NetworkAddress,
        InterfaceId,
        Metric
    ),
    RouteList3 = nsime_ipv4_static_routing:get_network_routes(RoutingState3),
    ?assert(lists:member(NetworkRoute3, RouteList3)),
    ?assert(lists:member(NetworkRoute2, RouteList3)),
    ?assert(lists:member(NetworkRoute1, RouteList3)),

    NetworkRoute4 = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        nsime_ipv4_mask:get_ones(),
        NextHop,
        InterfaceId,
        Metric
    ),
    ?assertNot(lists:member(NetworkRoute4, RouteList3)),
    ?assertNot(lists:member(NetworkRoute4, RouteList2)),
    ?assertNot(lists:member(NetworkRoute4, RouteList1)),
    RoutingState4 = nsime_ipv4_static_routing:add_host_route(
        RoutingState3,
        NetworkAddress,
        NextHop,
        InterfaceId,
        Metric
    ),
    RouteList4 = nsime_ipv4_static_routing:get_network_routes(RoutingState4),
    ?assert(lists:member(NetworkRoute4, RouteList4)),
    ?assert(lists:member(NetworkRoute3, RouteList4)),
    ?assert(lists:member(NetworkRoute2, RouteList4)),
    ?assert(lists:member(NetworkRoute1, RouteList4)),

    DefaultRoute = nsime_ipv4_routing_table_entry:create_network_route(
        nsime_ipv4_address:get_zero(),
        nsime_ipv4_mask:get_zero(),
        NextHop,
        InterfaceId,
        Metric
    ),
    ?assertNot(lists:member(DefaultRoute, RouteList4)),
    ?assertNot(lists:member(DefaultRoute, RouteList3)),
    ?assertNot(lists:member(DefaultRoute, RouteList2)),
    ?assertNot(lists:member(DefaultRoute, RouteList1)),
    RoutingState5 = nsime_ipv4_static_routing:set_default_route(
        RoutingState4,
        NextHop,
        InterfaceId,
        Metric
    ),
    RouteList5 = nsime_ipv4_static_routing:get_network_routes(RoutingState5),
    ?assert(lists:member(DefaultRoute, RouteList5)),
    ?assert(lists:member(NetworkRoute4, RouteList5)),
    ?assert(lists:member(NetworkRoute3, RouteList5)),
    ?assert(lists:member(NetworkRoute2, RouteList5)),
    ?assert(lists:member(NetworkRoute1, RouteList5)).

test_notify_interfaces_updown(_) ->
    {nsime_ipv4_static_routing, RoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(RoutingState, nsime_ipv4_static_routing_state)),
    InterfaceState = nsime_ipv4_interface:create(),
    ?assert(is_record(InterfaceState, nsime_ipv4_interface_state)),
    Address1 = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:create(Address1, Mask),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    InterfaceState1 = nsime_ipv4_interface:add_address(InterfaceState, AddressState1),
    AddressState2 = nsime_ipv4_interface_address:create(),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    InterfaceState2 = nsime_ipv4_interface:add_address(InterfaceState1, AddressState2),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingState), []),
    RoutingState1 = nsime_ipv4_static_routing:notify_interface_up(
        RoutingState,
        nsime_ipv4_interface:get_id(InterfaceState2),
        [InterfaceState2]
    ),
    InterfaceState3 = nsime_ipv4_interface:create(),
    Address2 = {10, 107, 2, 1},
    AddressState3 = nsime_ipv4_interface_address:create(Address2, Mask),
    ?assert(is_record(AddressState3, nsime_ipv4_interface_address_state)),
    InterfaceState4 = nsime_ipv4_interface:add_address(InterfaceState3, AddressState3),
    RoutingState2 = nsime_ipv4_static_routing:notify_interface_up(
        RoutingState1,
        nsime_ipv4_interface:get_id(InterfaceState4),
        [InterfaceState2, InterfaceState4]
    ),
    NetworkRoute1 = nsime_ipv4_routing_table_entry:create_network_route(
        nsime_ipv4_address:combine_mask(Address1, Mask),
        Mask,
        nsime_ipv4_interface:get_id(InterfaceState2),
        0
    ),
    NetworkRoute2 = nsime_ipv4_routing_table_entry:create_network_route(
        nsime_ipv4_address:combine_mask(Address2, Mask),
        Mask,
        nsime_ipv4_interface:get_id(InterfaceState4),
        0
    ),
    ?assert(
        lists:member(
            NetworkRoute1,
            nsime_ipv4_static_routing:get_network_routes(RoutingState2)
        )
    ),
    ?assert(
        lists:member(
            NetworkRoute2,
            nsime_ipv4_static_routing:get_network_routes(RoutingState2)
        )
    ),
    RoutingState3 = nsime_ipv4_static_routing:notify_interface_down(
        RoutingState2,
        nsime_ipv4_interface:get_id(InterfaceState1),
        [InterfaceState2, InterfaceState4]
    ),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingState3), [NetworkRoute2]),
    RoutingState4 = nsime_ipv4_static_routing:notify_interface_down(
        RoutingState3,
        nsime_ipv4_interface:get_id(InterfaceState4),
        [InterfaceState2, InterfaceState4]
    ),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingState4), []).

test_add_remove_addresses(_) ->
    {nsime_ipv4_static_routing, RoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(RoutingState, nsime_ipv4_static_routing_state)),
    InterfaceState = nsime_ipv4_interface:create(),
    AddressState = nsime_ipv4_interface_address:create(),
    InterfaceState1 = nsime_ipv4_interface:add_address(InterfaceState, AddressState),
    RoutingState1 = nsime_ipv4_static_routing:notify_add_address(
        RoutingState,
        nsime_ipv4_interface:get_id(InterfaceState1),
        AddressState,
        [InterfaceState1]
    ),
    InterfaceState2 = nsime_ipv4_interface:set_up(InterfaceState1),
    RoutingState2 = nsime_ipv4_static_routing:notify_add_address(
        RoutingState1,
        nsime_ipv4_interface:get_id(InterfaceState2),
        AddressState,
        [InterfaceState2]
    ),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingState2), []),
    Address = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:set_local_address(AddressState, Address),
    AddressState2 = nsime_ipv4_interface_address:set_mask(AddressState1, Mask),
    RoutingState3 = nsime_ipv4_static_routing:notify_add_address(
        RoutingState2,
        nsime_ipv4_interface:get_id(InterfaceState2),
        AddressState2,
        [InterfaceState2]
    ),
    NetworkRoute = nsime_ipv4_routing_table_entry:create_network_route(
        nsime_ipv4_address:combine_mask(Address, Mask),
        Mask,
        nsime_ipv4_interface:get_id(InterfaceState2),
        0
    ),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingState3), [NetworkRoute]),
    InterfaceState3 = nsime_ipv4_interface:set_down(InterfaceState2),
    RoutingState4 = nsime_ipv4_static_routing:add_host_route(
        RoutingState3,
        Address,
        nsime_ipv4_interface:get_id(InterfaceState3),
        0
    ),
    RoutingState5 = nsime_ipv4_static_routing:notify_remove_address(
        RoutingState4,
        nsime_ipv4_interface:get_id(InterfaceState3),
        AddressState2,
        [InterfaceState3]
    ),
    InterfaceState4 = nsime_ipv4_interface:set_up(InterfaceState3),
    RoutingState6 = nsime_ipv4_static_routing:notify_remove_address(
        RoutingState5,
        nsime_ipv4_interface:get_id(InterfaceState4),
        AddressState2,
        [InterfaceState4]
    ).

test_route_input(_) ->
    {nsime_ipv4_static_routing, RoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(RoutingState, nsime_ipv4_static_routing_state)),
    InterfaceState1 = nsime_ipv4_interface:create(),
    InterfaceId1 = nsime_ipv4_interface:get_id(InterfaceState1),
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    ?assertEqual(nsime_ptp_netdevice:set_interface(DevicePid, InterfaceId1), ok),
    InterfaceState2 = nsime_ipv4_interface:set_device(InterfaceState1, DevicePid),
    DestinationAddress1 = {224, 0, 0, 1},
    Ipv4Header1 = #nsime_ipv4_header{
        destination_address = DestinationAddress1
    },
    Packet = #nsime_packet{},
    ?assertEqual(
        catch nsime_ipv4_static_routing:route_input(
            RoutingState,
            Packet,
            Ipv4Header1,
            DevicePid,
            InterfaceState2,
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
    AddressState1 = nsime_ipv4_interface_address:create(DestinationAddress2, Mask1),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    InterfaceState3 = nsime_ipv4_interface:add_address(InterfaceState2, AddressState1),
    Ipv4Header2 = #nsime_ipv4_header{
        destination_address = DestinationAddress2
    },
    ?assertEqual(
        nsime_ipv4_static_routing:route_input(
            RoutingState,
            Packet,
            Ipv4Header2,
            DevicePid,
            InterfaceState3,
            junk,
            junk,
            junk,
            {erlang, date, []},
            []
        ),
        false
    ),
    ?assertEqual(
        nsime_ipv4_static_routing:route_input(
            RoutingState,
            Packet,
            Ipv4Header2,
            DevicePid,
            InterfaceState3,
            junk,
            junk,
            undefined,
            undefined,
            [InterfaceState3]
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
        nsime_ipv4_static_routing:route_input(
            RoutingState,
            Packet,
            Ipv4Header2,
            DevicePid,
            InterfaceState3,
            junk,
            junk,
            LocalDeliverCallback,
            ErrorCallback,
            [InterfaceState3]
        ),
        true
    ),
    nsime_simulator:run(),
    receive
        {local_deliver, Ref2} ->
            ok
    end,
    InterfaceState4 = nsime_ipv4_interface:set_forwarding(InterfaceState3, false),
    ?assertEqual(
        nsime_ipv4_static_routing:route_input(
            RoutingState,
            Packet,
            Ipv4Header2,
            DevicePid,
            InterfaceState4,
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

    InterfaceState5 = nsime_ipv4_interface:set_forwarding(InterfaceState4, true),
    Mask2 = {255, 255, 0, 0},
    AddressState2 = nsime_ipv4_interface_address:create(DestinationAddress2, Mask2),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    InterfaceState6 = nsime_ipv4_interface:set_up(InterfaceState5),
    RoutingState1 = nsime_ipv4_static_routing:notify_add_address(
        RoutingState,
        nsime_ipv4_interface:get_id(InterfaceState6),
        AddressState2,
        [InterfaceState6]
    ),

    Ref3 = make_ref(),
    UnicastForwardCallback = {
        ?MODULE,
        unicast_forward_tester,
        [self(), Ref3]
    },
    Ipv4Header3 = Ipv4Header2#nsime_ipv4_header{
        destination_address = {10, 107, 1, 100}
    },
    ?assertEqual(
        nsime_ipv4_static_routing:route_input(
            RoutingState1,
            Packet,
            Ipv4Header3,
            DevicePid,
            InterfaceState6,
            UnicastForwardCallback,
            junk,
            LocalDeliverCallback,
            ErrorCallback,
            [InterfaceState6]
        ),
        true
    ),
    nsime_simulator:run(),
    receive
        {unicast_forward, Ref3} ->
            ok
    end,

    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_route_output(_) ->
    {nsime_ipv4_static_routing, RoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(RoutingState, nsime_ipv4_static_routing_state)),
    InterfaceState1 = nsime_ipv4_interface:create(),
    Address1 = {10, 107, 1, 1},
    Mask1 = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:create(Address1, Mask1),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    InterfaceState2 = nsime_ipv4_interface:add_address(InterfaceState1, AddressState1),
    InterfaceId2 = nsime_ipv4_interface:get_id(InterfaceState2),
    RoutingState1 = nsime_ipv4_static_routing:notify_interface_up(
        RoutingState,
        InterfaceId2,
        [InterfaceState2]
    ),
    DestinationAddress = {224, 0, 0, 1},
    Ipv4Header = #nsime_ipv4_header{
        destination_address = DestinationAddress
    },
    Packet = #nsime_packet{},
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    ?assertEqual(nsime_ptp_netdevice:set_interface(DevicePid, InterfaceId2), ok),
    InterfaceState3 = nsime_ipv4_interface:set_device(InterfaceState2, DevicePid),
    Route = #nsime_ipv4_route{
        destination = DestinationAddress,
        source = Address1,
        gateway = nsime_ipv4_address:get_zero(),
        output_device = DevicePid
    },
    ?assertMatch(
        {error_noterror, Route},
        nsime_ipv4_static_routing:route_output(
            RoutingState1,
            Ipv4Header,
            DevicePid,
            [InterfaceState3]
        )
    ),
    DestinationAddress1 = {192, 168, 1, 1},
    ?assertMatch(
        {error_noroutetohost, undefined},
        nsime_ipv4_static_routing:route_output(
            RoutingState1,
            Ipv4Header#nsime_ipv4_header{
                destination_address = DestinationAddress1
            },
            DevicePid,
            [InterfaceState3]
        )
    ),
    ?assertMatch(
        {error_noroutetohost, undefined},
        nsime_ipv4_static_routing:route_output(
            RoutingState1,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            list_to_pid("<0.0.0>"),
            [InterfaceState3]
        )
    ),
    Mask2 = {255, 255, 0, 0},
    AddressState2 = nsime_ipv4_interface_address:create(Address1, Mask2),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    InterfaceState4 = nsime_ipv4_interface:set_up(InterfaceState3),
    RoutingState2 = nsime_ipv4_static_routing:notify_add_address(
        RoutingState1,
        nsime_ipv4_interface:get_id(InterfaceState4),
        AddressState2,
        [InterfaceState4]
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
        nsime_ipv4_static_routing:route_output(
            RoutingState2,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid,
            [InterfaceState4]
        )
    ),
    AddressState3 = nsime_ipv4_interface_address:set_secondary(AddressState1),
    RoutingState3 = nsime_ipv4_static_routing:notify_remove_address(
        RoutingState2,
        nsime_ipv4_interface:get_id(InterfaceState4),
        AddressState1,
        [InterfaceState4]
    ),
    RoutingState4 = nsime_ipv4_static_routing:notify_add_address(
        RoutingState3,
        nsime_ipv4_interface:get_id(InterfaceState4),
        AddressState3,
        [InterfaceState4]
    ),
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
        nsime_ipv4_static_routing:route_output(
            RoutingState4,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid,
            [InterfaceState4]
        )
    ),

    Mask3 = {255, 255, 255, 128},
    AddressState4 = nsime_ipv4_interface_address:create(Address1, Mask3),
    ?assert(is_record(AddressState4, nsime_ipv4_interface_address_state)),
    RoutingState5 = nsime_ipv4_static_routing:notify_add_address(
        RoutingState4,
        nsime_ipv4_interface:get_id(InterfaceState4),
        AddressState4,
        [InterfaceState4]
    ),
    AddressState5 = nsime_ipv4_interface_address:set_primary(AddressState3),
    RoutingState6 = nsime_ipv4_static_routing:notify_remove_address(
        RoutingState5,
        nsime_ipv4_interface:get_id(InterfaceState4),
        AddressState3,
        [InterfaceState4]
    ),
    RoutingState7 = nsime_ipv4_static_routing:notify_add_address(
        RoutingState6,
        nsime_ipv4_interface:get_id(InterfaceState4),
        AddressState5,
        [InterfaceState4]
    ),
    ?assertMatch(
        {error_noterror, Route3},
        nsime_ipv4_static_routing:route_output(
            RoutingState7,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid,
            [InterfaceState4]
        )
    ),
    AddressState6 = nsime_ipv4_interface_address:create(Address1, Mask3),
    ?assert(is_record(AddressState6, nsime_ipv4_interface_address_state)),
    RoutingState8 = nsime_ipv4_static_routing:notify_add_address(
        RoutingState7,
        nsime_ipv4_interface:get_id(InterfaceState4),
        AddressState6,
        [InterfaceState4]
    ),
    ?assertMatch(
        {error_noterror, Route3},
        nsime_ipv4_static_routing:route_output(
            RoutingState8,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid,
            [InterfaceState4]
        )
    ),

    Gateway1 = {10, 250, 1, 1},
    InterfaceId4 = nsime_ipv4_interface:get_id(InterfaceState4),
    RoutingState9 = nsime_ipv4_static_routing:add_host_route(
        RoutingState8,
        Address1,
        Gateway1,
        InterfaceId4,
        50
    ),
    Gateway2 = {10, 250, 1, 2},
    RoutingState10 = nsime_ipv4_static_routing:add_host_route(
        RoutingState9,
        Address1,
        Gateway2,
        InterfaceId4,
        10
    ),

    Route4 = #nsime_ipv4_route{
        destination = Address1,
        source = Address1,
        gateway = Gateway2,
        output_device = DevicePid
    },
    ?assertMatch(
        {error_noterror, Route4},
        nsime_ipv4_static_routing:route_output(
            RoutingState10,
            Ipv4Header#nsime_ipv4_header{
                destination_address = Address1
            },
            DevicePid,
            [InterfaceState4]
        )
    ).

%% Helper methods %%

error_callback_tester(From, Ref, _Packet, _Ipv4Header, _Type, _From, _Route) ->
    spawn(fun() -> From ! {error_callback, Ref} end).

local_deliver_tester(From, Ref, _Packet, _Header, _Interface) ->
    spawn(fun() -> From ! {local_deliver, Ref} end).

unicast_forward_tester(From, Ref, _Route, _Packet, _Header) ->
    spawn(fun() -> From ! {unicast_forward, Ref} end).
