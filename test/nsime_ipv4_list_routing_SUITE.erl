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

%% Purpose : Test module for nsime_ipv4_list_routing
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_list_routing_SUITE).
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
-include("nsime_ipv4_list_routing_state.hrl").
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").

all() -> [
            test_creation_shutdown,
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
    {nsime_ipv4_static_routing, StaticRoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(StaticRoutingState, nsime_ipv4_static_routing_state)),
    {nsime_ipv4_list_routing, ListRoutingState} = nsime_ipv4_list_routing:create(),
    ?assert(is_record(ListRoutingState, nsime_ipv4_list_routing_state)),
    {nsime_ipv4_list_routing, NewListRoutingState} = nsime_ipv4_list_routing:add_routing_protocol(
        {nsime_ipv4_list_routing, ListRoutingState},
        {nsime_ipv4_static_routing, StaticRoutingState},
        0
    ),
    ?assert(is_record(nsime_ipv4_list_routing:populate_network_routes(NewListRoutingState, []), nsime_ipv4_list_routing_state)).


test_notify_interfaces_updown(_) ->
    {nsime_ipv4_static_routing, StaticRoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(StaticRoutingState, nsime_ipv4_static_routing_state)),
    {nsime_ipv4_list_routing, ListRoutingState} = nsime_ipv4_list_routing:create(),
    ?assert(is_record(ListRoutingState, nsime_ipv4_list_routing_state)),
    {nsime_ipv4_list_routing, ListRoutingState1} = nsime_ipv4_list_routing:add_routing_protocol(
        {nsime_ipv4_list_routing, ListRoutingState},
        {nsime_ipv4_static_routing, StaticRoutingState},
        0
    ),

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
    ListRoutingState2 = nsime_ipv4_list_routing:notify_interface_up(
        ListRoutingState1,
        nsime_ipv4_interface:get_id(InterfaceState2),
        [InterfaceState2]
    ),
    ?assert(is_record(ListRoutingState2, nsime_ipv4_list_routing_state)),

    ListRoutingState3 = nsime_ipv4_list_routing:notify_interface_down(
        ListRoutingState2,
        nsime_ipv4_interface:get_id(InterfaceState2),
        [InterfaceState2]
    ),
    ?assert(is_record(ListRoutingState3, nsime_ipv4_list_routing_state)).

test_add_remove_addresses(_) ->
    {nsime_ipv4_static_routing, StaticRoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(StaticRoutingState, nsime_ipv4_static_routing_state)),
    {nsime_ipv4_list_routing, ListRoutingState} = nsime_ipv4_list_routing:create(),
    ?assert(is_record(ListRoutingState, nsime_ipv4_list_routing_state)),
    {nsime_ipv4_list_routing, ListRoutingState1} = nsime_ipv4_list_routing:add_routing_protocol(
        {nsime_ipv4_list_routing, ListRoutingState},
        {nsime_ipv4_static_routing, StaticRoutingState},
        0
    ),

    InterfaceState = nsime_ipv4_interface:create(),
    AddressState = nsime_ipv4_interface_address:create(),
    InterfaceState1 = nsime_ipv4_interface:add_address(InterfaceState, AddressState),
    ListRoutingState2 = nsime_ipv4_list_routing:notify_add_address(
        ListRoutingState1,
        nsime_ipv4_interface:get_id(InterfaceState1),
        AddressState,
        [InterfaceState1]
    ),
    ListRoutingState3 = nsime_ipv4_list_routing:notify_remove_address(
        ListRoutingState2,
        nsime_ipv4_interface:get_id(InterfaceState1),
        AddressState,
        [InterfaceState1]
    ).


test_route_input(_) ->
    {nsime_ipv4_static_routing, StaticRoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(StaticRoutingState, nsime_ipv4_static_routing_state)),
    {nsime_ipv4_list_routing, ListRoutingState} = nsime_ipv4_list_routing:create(),
    ?assert(is_record(ListRoutingState, nsime_ipv4_list_routing_state)),
    {nsime_ipv4_list_routing, ListRoutingState1} = nsime_ipv4_list_routing:add_routing_protocol(
        {nsime_ipv4_list_routing, ListRoutingState},
        {nsime_ipv4_static_routing, StaticRoutingState},
        0
    ),

    DevicePid = nsime_ptp_netdevice:create(),
    InterfaceState1 = nsime_ipv4_interface:create(),
    DestinationAddress1 = {224, 0, 0, 1},
    Ipv4Header1 = #nsime_ipv4_header{
        destination_address = DestinationAddress1
    },
    Packet = #nsime_packet{},
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
    ?assertEqual(
        nsime_ipv4_list_routing:route_input(
            ListRoutingState1,
            Packet,
            Ipv4Header1,
            DevicePid,
            InterfaceState1,
            junk,
            junk,
            LocalDeliverCallback,
            ErrorCallback,
            []
        ),
        options_not_supported
    ),
    receive
        {local_deliver, Ref2} ->
            ok
    end,
    InterfaceState2 = nsime_ipv4_interface:set_forwarding(InterfaceState1, false),
    ?assertEqual(
        nsime_ipv4_list_routing:route_input(
            ListRoutingState1,
            Packet,
            Ipv4Header1,
            DevicePid,
            InterfaceState2,
            junk,
            junk,
            LocalDeliverCallback,
            ErrorCallback,
            []
        ),
        false
    ),
    receive
        {error_callback, Ref1} ->
            ok
    end,
    Ipv4Header2 = #nsime_ipv4_header{
        destination_address = {10, 107, 1, 100}
    },
    ?assertEqual(
        nsime_ipv4_list_routing:route_input(
            ListRoutingState1,
            Packet,
            Ipv4Header2,
            DevicePid,
            InterfaceState2,
            junk,
            junk,
            LocalDeliverCallback,
            ErrorCallback,
            [],
            false
        ),
        false
    ),
    receive
        {error_callback, Ref1} ->
            ok
    end,
    ?assertEqual(
        nsime_ipv4_list_routing:route_input(
            ListRoutingState1,
            Packet,
            Ipv4Header2,
            DevicePid,
            InterfaceState1,
            junk,
            junk,
            LocalDeliverCallback,
            ErrorCallback,
            [],
            false
        ),
        false
    ).

test_route_output(_) ->
    {nsime_ipv4_static_routing, StaticRoutingState} = nsime_ipv4_static_routing:create(),
    ?assert(is_record(StaticRoutingState, nsime_ipv4_static_routing_state)),
    {nsime_ipv4_list_routing, ListRoutingState} = nsime_ipv4_list_routing:create(),
    ?assert(is_record(ListRoutingState, nsime_ipv4_list_routing_state)),
    {nsime_ipv4_list_routing, ListRoutingState1} = nsime_ipv4_list_routing:add_routing_protocol(
        {nsime_ipv4_list_routing, ListRoutingState},
        {nsime_ipv4_static_routing, StaticRoutingState},
        0
    ),

    InterfaceState1 = nsime_ipv4_interface:create(),
    Address1 = {10, 107, 1, 1},
    Mask1 = {255, 255, 255, 0},
    AddressState1 = nsime_ipv4_interface_address:create(Address1, Mask1),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    InterfaceState2 = nsime_ipv4_interface:add_address(InterfaceState1, AddressState1),
    InterfaceId2 = nsime_ipv4_interface:get_id(InterfaceState2),
    ListRoutingState2 = nsime_ipv4_list_routing:notify_interface_up(
        ListRoutingState1,
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
        nsime_ipv4_list_routing:route_output(
            ListRoutingState2,
            Ipv4Header,
            DevicePid,
            [InterfaceState3]
        )
    ).

%% Helper methods %%

error_callback_tester(From, Ref, _Packet, _Ipv4Header, _Type) ->
    spawn(fun() -> From ! {error_callback, Ref} end).

local_deliver_tester(From, Ref, _Packet, _Header, _Interface) ->
    spawn(fun() -> From ! {local_deliver, Ref} end).

unicast_forward_tester(From, Ref, _Route, _Packet, _Header) ->
    spawn(fun() -> From ! {unicast_forward, Ref} end).
