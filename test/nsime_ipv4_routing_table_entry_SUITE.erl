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

%% Purpose : Test module for nsime_ipv4_routing_table_entry
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_routing_table_entry_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include("nsime_types.hrl").
-include("nsime_ipv4_routing_table_entry.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [
          {group, testgroup_all}
         ].

groups() ->
    [{
        testgroup_all,
        [parallel],
        [
          test_creation,
          test_properties
        ]
    }].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(testgroup_all, Config) ->
    Config.

end_per_group(testgroup_all, Config) ->
    Config.

test_creation(_) ->
    Destination = {10, 107, 1, 1},
    InterfacePid = list_to_pid("<0.0.0>"),
    Metric = 5,
    HostRoute = #nsime_ipv4_routing_table_entry{
        destination = Destination,
        network_mask = nsime_ipv4_mask:get_ones(),
        gateway = nsime_ipv4_address:get_zero(),
        interface = InterfacePid,
        metric = Metric
    },
    ?assertEqual(
        nsime_ipv4_routing_table_entry:create_host_route(
            Destination,
            InterfacePid,
            Metric
        ),
        HostRoute
    ),
    NextHop = {10, 250, 1, 1},
    AnotherHostRoute = HostRoute#nsime_ipv4_routing_table_entry{
        gateway = NextHop
    },
    ?assertEqual(
        nsime_ipv4_routing_table_entry:create_host_route(
            Destination,
            NextHop,
            InterfacePid,
            Metric
        ),
        AnotherHostRoute
    ),

    NetworkMask = {255, 255, 255, 0},
    NetworkRoute = #nsime_ipv4_routing_table_entry{
        destination = Destination,
        network_mask = NetworkMask,
        gateway = nsime_ipv4_address:get_zero(),
        interface = InterfacePid,
        metric = Metric
    },
    ?assertEqual(
        nsime_ipv4_routing_table_entry:create_network_route(
            Destination,
            NetworkMask,
            InterfacePid,
            Metric
        ),
        NetworkRoute
    ),
    AnotherNetworkRoute = NetworkRoute#nsime_ipv4_routing_table_entry{
        gateway = NextHop
    },
    ?assertEqual(
        nsime_ipv4_routing_table_entry:create_network_route(
            Destination,
            NetworkMask,
            NextHop,
            InterfacePid,
            Metric
        ),
        AnotherNetworkRoute
    ),
    DefaultRoute = #nsime_ipv4_routing_table_entry{
        destination = nsime_ipv4_address:get_zero(),
        network_mask = nsime_ipv4_mask:get_ones(),
        gateway = NextHop,
        interface = InterfacePid
    },
    ?assertEqual(
        nsime_ipv4_routing_table_entry:create_default_route(
            NextHop,
            InterfacePid
        ),
        DefaultRoute
    ).

test_properties(_) ->
    Destination = {10, 107, 1, 1},
    InterfacePid = list_to_pid("<0.0.0>"),
    Metric = 5,
    HostRoute = nsime_ipv4_routing_table_entry:create_host_route(
        Destination,
        InterfacePid,
        Metric
    ),
    ?assert(nsime_ipv4_routing_table_entry:is_host(HostRoute)),
    ?assertNot(nsime_ipv4_routing_table_entry:is_network(HostRoute)),
    ?assertNot(nsime_ipv4_routing_table_entry:is_gateway(HostRoute)),
    ?assertNot(nsime_ipv4_routing_table_entry:is_default(HostRoute)),
    NetworkRoute = HostRoute#nsime_ipv4_routing_table_entry{
        network_mask = {255, 255, 255, 0}
    },
    ?assert(nsime_ipv4_routing_table_entry:is_network(NetworkRoute)),
    ?assertNot(nsime_ipv4_routing_table_entry:is_host(NetworkRoute)),
    ?assertNot(nsime_ipv4_routing_table_entry:is_gateway(NetworkRoute)),
    ?assertNot(nsime_ipv4_routing_table_entry:is_default(NetworkRoute)),
    DefaultRoute = HostRoute#nsime_ipv4_routing_table_entry{
        destination = nsime_ipv4_address:get_zero()
    },
    ?assert(nsime_ipv4_routing_table_entry:is_default(DefaultRoute)),
    GatewayRoute = HostRoute#nsime_ipv4_routing_table_entry{
        gateway = {10, 107, 1, 1}
    },
    ?assert(nsime_ipv4_routing_table_entry:is_gateway(GatewayRoute)).
