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

%% Purpose : Test module for nsime_ipv4_interface
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_interface_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
            test_add_remove_addresses
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    InterfaceState = nsime_ipv4_interface:create(),
    ?assert(is_record(InterfaceState, nsime_ipv4_interface_state)).

test_set_get_components(_) ->
    InterfaceState = nsime_ipv4_interface:create(),
    ?assert(is_record(InterfaceState, nsime_ipv4_interface_state)),

    DevicePid = list_to_pid("<0.0.1>"),
    InterfaceState1 = nsime_ipv4_interface:set_device(InterfaceState, DevicePid),
    ?assert(is_record(InterfaceState1, nsime_ipv4_interface_state)),
    ?assertEqual(nsime_ipv4_interface:get_device(InterfaceState1), DevicePid),

    ArpCachePid = list_to_pid("<0.0.2>"),
    InterfaceState2 = nsime_ipv4_interface:set_arp_cache(InterfaceState1, ArpCachePid),
    ?assert(is_record(InterfaceState2, nsime_ipv4_interface_state)),
    ?assertEqual(nsime_ipv4_interface:get_arp_cache(InterfaceState2), ArpCachePid),

    Metric = 5,
    InterfaceState3 = nsime_ipv4_interface:set_metric(InterfaceState2, Metric),
    ?assertEqual(nsime_ipv4_interface:get_metric(InterfaceState3), Metric),

    ?assertNot(nsime_ipv4_interface:is_up(InterfaceState3)),
    ?assert(nsime_ipv4_interface:is_forwarding(InterfaceState3)),
    ?assert(nsime_ipv4_interface:is_down(InterfaceState3)),

    InterfaceState4 = nsime_ipv4_interface:set_up(InterfaceState3),
    ?assert(nsime_ipv4_interface:is_up(InterfaceState4)),
    ?assertNot(nsime_ipv4_interface:is_down(InterfaceState4)),

    InterfaceState5 = nsime_ipv4_interface:set_down(InterfaceState4),
    ?assertNot(nsime_ipv4_interface:is_up(InterfaceState5)),

    InterfaceState6 = nsime_ipv4_interface:set_forwarding(InterfaceState5, false),
    ?assertNot(nsime_ipv4_interface:is_forwarding(InterfaceState6)).

test_add_remove_addresses(_) ->
    InterfaceState = nsime_ipv4_interface:create(),
    ?assert(is_record(InterfaceState, nsime_ipv4_interface_state)),
    AddressState1 = nsime_ipv4_interface_address:create(),
    InterfaceState1 = nsime_ipv4_interface:add_address(InterfaceState, AddressState1),
    ?assertEqual(nsime_ipv4_interface:get_address_list(InterfaceState1), [AddressState1]),
    AddressState2 = nsime_ipv4_interface_address:create(),
    InterfaceState2 = nsime_ipv4_interface:add_address(InterfaceState1, AddressState2),
    AddressList = nsime_ipv4_interface:get_address_list(InterfaceState2),
    ?assert(lists:member(AddressState1, AddressList)),
    ?assert(lists:member(AddressState2, AddressList)),
    InterfaceState3 = nsime_ipv4_interface:remove_address(InterfaceState2, AddressState1),
    AddressList1 = nsime_ipv4_interface:get_address_list(InterfaceState3),
    ?assertNot(lists:member(AddressState1, AddressList1)),
    ?assert(lists:member(AddressState2, AddressList1)).
