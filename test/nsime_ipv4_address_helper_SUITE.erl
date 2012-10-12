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

%% Purpose : Test module for nsime_ipv4_address_helper
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_address_helper_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_ipv4_address_helper_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_base,
            test_assign,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    HelperPid = nsime_ipv4_address_helper:create(),
    ?assert(is_pid(HelperPid)),
    ?assertEqual(nsime_ipv4_address_helper:destroy(HelperPid), stopped).

test_set_base(_) ->
    HelperPid = nsime_ipv4_address_helper:create(),
    ?assert(is_pid(HelperPid)),
    ?assertEqual(nsime_ipv4_address_helper:set_base(HelperPid, "10.107.1.0", "255.255.255.0"), ok),
    ?assertEqual(nsime_ipv4_address_helper:new_address(HelperPid), {10, 107, 1, 1}),
    ?assertEqual(nsime_ipv4_address_helper:new_network(HelperPid), {10, 107, 2, 0}),
    ?assertEqual(nsime_ipv4_address_helper:set_base(HelperPid, "192.168.1.0", "255.255.255.0", "0.0.0.254"), ok),
    ?assertEqual(nsime_ipv4_address_helper:new_address(HelperPid), {192, 168, 1, 254}),
    ?assertError(nsime_address_overflow, nsime_ipv4_address_helper:new_address(HelperPid)),
    ?assertEqual(nsime_ipv4_address_helper:new_network(HelperPid), {192, 168, 2, 0}),
    ?assertEqual(nsime_ipv4_address_helper:destroy(HelperPid), stopped).

test_assign(_) ->
    HelperPid = nsime_ipv4_address_helper:create(),
    ?assert(is_pid(HelperPid)),
    ?assertEqual(nsime_ipv4_address_helper:set_base(HelperPid, "10.107.1.0", "255.255.255.0"), ok),
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    NodePidList = nsime_node:create(2),

    PtpHelperPid = nsime_ptp_helper:create(),
    ?assert(is_pid(PtpHelperPid)),
    ?assertEqual(nsime_ptp_helper:call_on_device(PtpHelperPid, set_data_rate, {5, mega_bits_per_sec}), ok),
    ?assertEqual(nsime_ptp_helper:call_on_channel(PtpHelperPid, set_channel_delay, {2, milli_sec}), ok),

    DevicePidList = nsime_ptp_helper:install(PtpHelperPid, NodePidList),
    ?assertEqual(nsime_internet_stack_helper:install(NodePidList), ok),
    InterfacePidList = nsime_ipv4_address_helper:assign(HelperPid, DevicePidList),
    ?assertEqual(nsime_ipv4_address_helper:set_base(HelperPid, "192.168.1.0", "255.255.255.0", "0.0.0.254"), ok),
    ?assertError(nsime_address_overflow, nsime_ipv4_address_helper:assign(HelperPid, DevicePidList)),
    ?assertEqual(nsime_simulator:run(), simulation_complete),
    ?assertEqual(nsime_ptp_helper:destroy(PtpHelperPid), stopped),
    ?assertEqual(nsime_ipv4_address_helper:destroy(HelperPid), stopped),
    nsime_simulator:stop().

test_cast_info_codechange(_) ->
    HelperPid = nsime_ipv4_address_helper:create(),
    ?assert(is_pid(HelperPid)),
    gen_server:cast(HelperPid, junk),
    HelperPid ! junk,
    nsime_ipv4_address_helper:code_change(junk, junk, junk),
    ?assertEqual(nsime_ipv4_address_helper:destroy(HelperPid), stopped).
