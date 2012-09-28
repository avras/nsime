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
-include("nsime_ipv4_interface_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
            test_add_remove_addresses,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    InterfacePid = nsime_ipv4_interface:create(),
    ?assert(is_pid(InterfacePid)),
    ?assertEqual(nsime_ipv4_interface:destroy(InterfacePid), stopped).

test_set_get_components(_) ->
    InterfacePid = nsime_ipv4_interface:create(),
    ?assert(is_pid(InterfacePid)),
    NodePid = list_to_pid("<0.0.0>"),
    ?assertEqual(
        nsime_ipv4_interface:set_node(
            InterfacePid,
            NodePid
        ),
        ok
    ),
    DevicePid = list_to_pid("<0.0.1>"),
    ?assertEqual(
        nsime_ipv4_interface:set_device(
            InterfacePid,
            DevicePid
        ),
        ok
    ),
    ?assertEqual(
        nsime_ipv4_interface:get_device(InterfacePid),
        DevicePid
    ),
    ArpCachePid = list_to_pid("<0.0.2>"),
    ?assertEqual(
        nsime_ipv4_interface:set_arp_cache(
            InterfacePid,
            ArpCachePid
        ),
        ok
    ),
    ?assertEqual(
        nsime_ipv4_interface:get_arp_cache(InterfacePid),
        ArpCachePid
    ),
    Metric = 5,
    ?assertEqual(
        nsime_ipv4_interface:set_metric(
            InterfacePid,
            Metric
        ),
        ok
    ),
    ?assertEqual(
        nsime_ipv4_interface:get_metric(InterfacePid),
        Metric
    ),
    ?assertNot(nsime_ipv4_interface:is_up(InterfacePid)),
    ?assert(nsime_ipv4_interface:is_forwarding(InterfacePid)),
    ?assert(nsime_ipv4_interface:is_down(InterfacePid)),
    ?assertEqual(nsime_ipv4_interface:set_up(InterfacePid), ok),
    ?assert(nsime_ipv4_interface:is_up(InterfacePid)),
    ?assertNot(nsime_ipv4_interface:is_down(InterfacePid)),
    ?assertEqual(nsime_ipv4_interface:set_down(InterfacePid), ok),
    ?assertNot(nsime_ipv4_interface:is_up(InterfacePid)),
    ?assertEqual(nsime_ipv4_interface:set_forwarding(InterfacePid, false), ok),
    ?assertNot(nsime_ipv4_interface:is_forwarding(InterfacePid)),
    ?assertEqual(nsime_ipv4_interface:destroy(InterfacePid), stopped).

test_add_remove_addresses(_) ->
    InterfacePid = nsime_ipv4_interface:create(),
    ?assert(is_pid(InterfacePid)),
    AddressPid1 = nsime_ipv4_interface_address:create(),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid, AddressPid1), ok),
    ?assertEqual(nsime_ipv4_interface:get_address_list(InterfacePid), [AddressPid1]),
    AddressPid2 = nsime_ipv4_interface_address:create(),
    ?assertEqual(nsime_ipv4_interface:add_address(InterfacePid, AddressPid2), ok),
    AddressList = nsime_ipv4_interface:get_address_list(InterfacePid),
    ?assert(lists:member(AddressPid1, AddressList)),
    ?assert(lists:member(AddressPid2, AddressList)),
    ?assertEqual(nsime_ipv4_interface:remove_address(InterfacePid, AddressPid1), ok),
    AddressList1 = nsime_ipv4_interface:get_address_list(InterfacePid),
    ?assertNot(lists:member(AddressPid1, AddressList1)),
    ?assert(lists:member(AddressPid2, AddressList1)),
    ?assertEqual(nsime_ipv4_interface_address:destroy(AddressPid1), stopped),
    ?assertEqual(nsime_ipv4_interface:destroy(InterfacePid), stopped).

test_cast_info_codechange(_) ->
    InterfacePid = nsime_ipv4_interface:create(),
    ?assert(is_pid(InterfacePid)),
    gen_server:cast(InterfacePid, junk),
    InterfacePid ! junk,
    nsime_ipv4_interface:code_change(junk, junk, junk),
    ?assertEqual(nsime_ipv4_interface:destroy(InterfacePid), stopped).
