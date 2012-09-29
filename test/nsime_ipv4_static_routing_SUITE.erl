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
-include("nsime_ipv4_routing_table_entry.hrl").
-include("nsime_ipv4_static_routing_state.hrl").

all() -> [
            test_creation_shutdown,
            test_add_routes,
            test_notify_interfaces_updown,
            test_add_remove_addresses,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assert(is_pid(RoutingPid)),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingPid), []),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped).

test_add_routes(_) ->
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assert(is_pid(RoutingPid)),
    InterfacePid = nsime_ipv4_interface:create(),
    NetworkAddress = {10, 107, 1, 1},
    NetworkMask = {255, 255, 255, 0},
    Metric = 5,
    NetworkRoute1 = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        NetworkMask,
        InterfacePid,
        Metric
    ),
    ?assertEqual(
        nsime_ipv4_static_routing:add_network_route(
            RoutingPid,
            NetworkAddress,
            NetworkMask,
            InterfacePid,
            Metric
        ),
        ok
    ),
    RouteList1 = nsime_ipv4_static_routing:get_network_routes(RoutingPid),
    ?assert(lists:member(NetworkRoute1, RouteList1)),
    NextHop = {10, 250, 1, 1},
    NetworkRoute2 = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        NetworkMask,
        NextHop,
        InterfacePid,
        Metric
    ),
    ?assertNot(lists:member(NetworkRoute2, RouteList1)),
    ?assertEqual(
        nsime_ipv4_static_routing:add_network_route(
            RoutingPid,
            NetworkAddress,
            NetworkMask,
            NextHop,
            InterfacePid,
            Metric
        ),
        ok
    ),
    RouteList2 = nsime_ipv4_static_routing:get_network_routes(RoutingPid),
    ?assert(lists:member(NetworkRoute2, RouteList2)),
    ?assert(lists:member(NetworkRoute1, RouteList2)),

    NetworkRoute3 = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        nsime_ipv4_mask:get_ones(),
        InterfacePid,
        Metric
    ),
    ?assertNot(lists:member(NetworkRoute3, RouteList2)),
    ?assertNot(lists:member(NetworkRoute3, RouteList1)),
    ?assertEqual(
        nsime_ipv4_static_routing:add_host_route(
            RoutingPid,
            NetworkAddress,
            InterfacePid,
            Metric
        ),
        ok
    ),
    RouteList3 = nsime_ipv4_static_routing:get_network_routes(RoutingPid),
    ?assert(lists:member(NetworkRoute3, RouteList3)),
    ?assert(lists:member(NetworkRoute2, RouteList3)),
    ?assert(lists:member(NetworkRoute1, RouteList3)),

    NetworkRoute4 = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        nsime_ipv4_mask:get_ones(),
        NextHop,
        InterfacePid,
        Metric
    ),
    ?assertNot(lists:member(NetworkRoute4, RouteList3)),
    ?assertNot(lists:member(NetworkRoute4, RouteList2)),
    ?assertNot(lists:member(NetworkRoute4, RouteList1)),
    ?assertEqual(
        nsime_ipv4_static_routing:add_host_route(
            RoutingPid,
            NetworkAddress,
            NextHop,
            InterfacePid,
            Metric
        ),
        ok
    ),
    RouteList4 = nsime_ipv4_static_routing:get_network_routes(RoutingPid),
    ?assert(lists:member(NetworkRoute4, RouteList4)),
    ?assert(lists:member(NetworkRoute3, RouteList4)),
    ?assert(lists:member(NetworkRoute2, RouteList4)),
    ?assert(lists:member(NetworkRoute1, RouteList4)),

    DefaultRoute = nsime_ipv4_routing_table_entry:create_network_route(
        nsime_ipv4_address:get_zero(),
        nsime_ipv4_mask:get_zero(),
        NextHop,
        InterfacePid,
        Metric
    ),
    ?assertNot(lists:member(DefaultRoute, RouteList4)),
    ?assertNot(lists:member(DefaultRoute, RouteList3)),
    ?assertNot(lists:member(DefaultRoute, RouteList2)),
    ?assertNot(lists:member(DefaultRoute, RouteList1)),
    ?assertEqual(
        nsime_ipv4_static_routing:set_default_route(
            RoutingPid,
            NextHop,
            InterfacePid,
            Metric
        ),
        ok
    ),
    RouteList5 = nsime_ipv4_static_routing:get_network_routes(RoutingPid),
    ?assert(lists:member(DefaultRoute, RouteList5)),
    ?assert(lists:member(NetworkRoute4, RouteList5)),
    ?assert(lists:member(NetworkRoute3, RouteList5)),
    ?assert(lists:member(NetworkRoute2, RouteList5)),
    ?assert(lists:member(NetworkRoute1, RouteList5)),

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
        nsime_ipv4_static_routing:notify_interface_up(
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
        nsime_ipv4_static_routing:notify_interface_up(
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
        nsime_ipv4_static_routing:notify_interface_down(
            RoutingPid,
            InterfacePid1
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_static_routing:get_network_routes(RoutingPid), [NetworkRoute2]),
    ?assertEqual(
        nsime_ipv4_static_routing:notify_interface_down(
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
        nsime_ipv4_static_routing:notify_add_address(
            RoutingPid,
            InterfacePid,
            AddressPid
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_interface:set_up(InterfacePid), ok),
    ?assertEqual(
        nsime_ipv4_static_routing:notify_add_address(
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
        nsime_ipv4_static_routing:notify_add_address(
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
        nsime_ipv4_static_routing:notify_remove_address(
            RoutingPid,
            InterfacePid,
            AddressPid
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_interface:set_up(InterfacePid), ok),
    ?assertEqual(
        nsime_ipv4_static_routing:notify_remove_address(
            RoutingPid,
            InterfacePid,
            AddressPid
        ),
        ok
    ),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped).

test_cast_info_codechange(_) ->
    RoutingPid = nsime_ipv4_static_routing:create(),
    ?assert(is_pid(RoutingPid)),
    gen_server:cast(RoutingPid, junk),
    RoutingPid ! junk,
    nsime_ipv4_static_routing:code_change(junk, junk, junk),
    ?assertEqual(nsime_ipv4_static_routing:destroy(RoutingPid), stopped).
