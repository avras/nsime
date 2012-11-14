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

%% Purpose : Test module for nsime_ipv4_interface_address
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_interface_address_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_ipv4_interface_address_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    AddressState1 = nsime_ipv4_interface_address:create(),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),

    Address = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressState2 = nsime_ipv4_interface_address:create(Address, Mask),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    ?assertEqual(
        nsime_ipv4_interface_address:get_local_address(AddressState2),
        Address
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:get_mask(AddressState2),
        Mask
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:get_broadcast_address(AddressState2),
        {10, 107, 1, 255}
    ).

test_set_get_components(_) ->
    AddressState1 = nsime_ipv4_interface_address:create(),
    ?assert(is_record(AddressState1, nsime_ipv4_interface_address_state)),
    Address = {10, 107, 1, 1},
    AddressState2 = nsime_ipv4_interface_address:set_local_address(AddressState1, Address),
    ?assert(is_record(AddressState2, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_interface_address:get_local_address(AddressState2), Address),
    BroadcastAddress = {10, 107, 1, 255},
    AddressState3 = nsime_ipv4_interface_address:set_broadcast_address(AddressState2, BroadcastAddress),
    ?assert(is_record(AddressState3, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_interface_address:get_broadcast_address(AddressState3), BroadcastAddress),
    Mask = {255, 255, 255, 0},
    AddressState4 = nsime_ipv4_interface_address:set_mask(AddressState3, Mask),
    ?assert(is_record(AddressState4, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_interface_address:get_mask(AddressState4), Mask),
    AddressState5 = nsime_ipv4_interface_address:set_address_scope(AddressState4, link),
    ?assert(is_record(AddressState5, nsime_ipv4_interface_address_state)),
    ?assertEqual(nsime_ipv4_interface_address:get_address_scope(AddressState5), link),
    ?assertNot(nsime_ipv4_interface_address:is_secondary(AddressState5)),
    AddressState6 = nsime_ipv4_interface_address:set_secondary(AddressState5),
    ?assert(nsime_ipv4_interface_address:is_secondary(AddressState6)),
    AddressState7 = nsime_ipv4_interface_address:set_primary(AddressState6),
    ?assertNot(nsime_ipv4_interface_address:is_secondary(AddressState7)).
