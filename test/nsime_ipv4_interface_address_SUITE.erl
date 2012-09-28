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
            test_set_get_components,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    AddressPid1 = nsime_ipv4_interface_address:create(),
    ?assert(is_pid(AddressPid1)),
    AddressState = #nsime_ipv4_interface_address_state{
        local_address = nsime_ipv4_address:get_any(),
        broadcast_address = nsime_ipv4_address:get_broadcast()
    },

    AddressPid2 = nsime_ipv4_interface_address:create(AddressState),
    ?assert(is_pid(AddressPid2)),
    ?assertEqual(
        nsime_ipv4_interface_address:get_local_address(AddressPid2),
        nsime_ipv4_address:get_any()
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:get_broadcast_address(AddressPid2),
        nsime_ipv4_address:get_broadcast()
    ),

    Address = {10, 107, 1, 1},
    Mask = {255, 255, 255, 0},
    AddressPid3 = nsime_ipv4_interface_address:create(Address, Mask),
    ?assert(is_pid(AddressPid3)),
    ?assertEqual(
        nsime_ipv4_interface_address:get_local_address(AddressPid3),
        Address
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:get_mask(AddressPid3),
        Mask
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:get_broadcast_address(AddressPid3),
        {10, 107, 1, 255}
    ),

    ?assertEqual(nsime_ipv4_interface_address:destroy(AddressPid3), stopped),
    ?assertEqual(nsime_ipv4_interface_address:destroy(AddressPid2), stopped),
    ?assertEqual(nsime_ipv4_interface_address:destroy(AddressPid1), stopped).

test_set_get_components(_) ->
    AddressPid1 = nsime_ipv4_interface_address:create(),
    ?assert(is_pid(AddressPid1)),
    Address = {10, 107, 1, 1},
    ?assertEqual(
        nsime_ipv4_interface_address:set_local_address(
            AddressPid1,
            Address
        ),
        ok
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:get_local_address(AddressPid1),
        Address
    ),
    BroadcastAddress = {10, 107, 1, 255},
    ?assertEqual(
        nsime_ipv4_interface_address:set_broadcast_address(
            AddressPid1,
            BroadcastAddress
        ),
        ok
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:get_broadcast_address(AddressPid1),
        BroadcastAddress
    ),
    Mask = {255, 255, 255, 0},
    ?assertEqual(
        nsime_ipv4_interface_address:set_mask(
            AddressPid1,
            Mask
        ),
        ok
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:get_mask(AddressPid1),
        Mask
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:set_address_scope(
            AddressPid1,
            link
        ),
        ok
    ),
    ?assertEqual(
        nsime_ipv4_interface_address:get_address_scope(AddressPid1),
        link
    ),
    ?assertNot(nsime_ipv4_interface_address:is_secondary(AddressPid1)),
    ?assertEqual(nsime_ipv4_interface_address:set_secondary(AddressPid1), ok),
    ?assert(nsime_ipv4_interface_address:is_secondary(AddressPid1)),
    ?assertEqual(nsime_ipv4_interface_address:set_primary(AddressPid1), ok),
    ?assertNot(nsime_ipv4_interface_address:is_secondary(AddressPid1)),
    ?assertEqual(nsime_ipv4_interface_address:destroy(AddressPid1), stopped).

test_cast_info_codechange(_) ->
    AddressPid = nsime_ipv4_interface_address:create(),
    ?assert(is_pid(AddressPid)),
    gen_server:cast(AddressPid, junk),
    AddressPid ! junk,
    nsime_ipv4_interface_address:code_change(junk, junk, junk),
    ?assertEqual(nsime_ipv4_interface_address:destroy(AddressPid), stopped).
