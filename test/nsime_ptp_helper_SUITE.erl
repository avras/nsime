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

%% Purpose : Test module for nsime_ptp_helper
%% Author : Saravanan Vijayakumaran

-module(nsime_ptp_helper_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_ptp_helper_state.hrl").

all() -> [
            test_creation_shutdown,
            test_install,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    HelperPid = nsime_ptp_helper:create(),
    ?assert(is_pid(HelperPid)),
    ?assertEqual(nsime_ptp_helper:destroy(HelperPid), stopped).

test_install(_) ->
    HelperPid = nsime_ptp_helper:create(),
    ?assert(is_pid(HelperPid)),
    DataRate = {5, mega_bits_per_sec},
    InterframeGap = {1, sec},
    ?assertEqual(nsime_ptp_helper:call_on_device(HelperPid, set_data_rate, DataRate), ok),
    ?assertEqual(nsime_ptp_helper:call_on_device(HelperPid, set_interframe_gap, InterframeGap), ok),
    Delay = {5, micro_sec},
    ?assertEqual(nsime_ptp_helper:call_on_channel(HelperPid, set_channel_delay, Delay), ok),
    ?assertError(
        invalid_argument,
        nsime_ptp_helper:install(HelperPid, [1,2,3])
    ),
    [NodePid1, NodePid2] = nsime_node:create(2),
    [DevicePid1, DevicePid2] = nsime_ptp_helper:install(HelperPid, [NodePid1, NodePid2]),
    ?assert(is_pid(DevicePid1)),
    ?assert(is_pid(DevicePid2)),
    ChannelPid1 = nsime_ptp_netdevice:get_channel(DevicePid1),
    ChannelPid2 = nsime_ptp_netdevice:get_channel(DevicePid2),
    ?assert(ChannelPid1 == ChannelPid2),
    ?assertEqual(nsime_ptp_channel:get_channel_delay(ChannelPid1), Delay),
    ?assertEqual(nsime_ptp_helper:destroy(HelperPid), stopped).

test_cast_info_codechange(_) ->
    HelperPid = nsime_ptp_helper:create(),
    ?assert(is_pid(HelperPid)),
    gen_server:cast(HelperPid, junk),
    HelperPid ! junk,
    nsime_ptp_helper:code_change(junk, junk, junk),
    ?assertEqual(nsime_ptp_helper:destroy(HelperPid), stopped).
