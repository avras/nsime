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

%% Purpose : Test module for nsime_udp_echo_server
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_echo_server_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_udp_echo_server_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
            test_start,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    ServerPid = nsime_udp_echo_server:create(),
    ?assert(is_pid(ServerPid)),
    ?assertEqual(nsime_udp_echo_server:destroy(ServerPid), stopped).

test_set_get_components(_) ->
    ServerPid = nsime_udp_echo_server:create(),
    ?assert(is_pid(ServerPid)),
    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),

    ?assertEqual(nsime_udp_echo_server:set_node(ServerPid, NodePid), ok),
    ?assertEqual(nsime_udp_echo_server:get_node(ServerPid), NodePid),
    ?assertEqual(nsime_udp_echo_server:set_listen_port(ServerPid, 9), ok),
    ?assertError(invalid_argument, nsime_udp_echo_server:set_listen_port(ServerPid, -1)),
    ?assertError(invalid_argument, nsime_udp_echo_server:set_listen_port(ServerPid, 65536)),

    Callback1 = {erlang, date, [1]},
    ?assertEqual(nsime_udp_echo_server:set_transmit_trace_callback(ServerPid, Callback1), ok),
    ?assertEqual(nsime_udp_echo_server:get_transmit_trace_callback(ServerPid), Callback1),
    Callback2 = {erlang, date, [2]},
    ?assertEqual(nsime_udp_echo_server:set_receive_trace_callback(ServerPid, Callback2), ok),
    ?assertEqual(nsime_udp_echo_server:get_receive_trace_callback(ServerPid), Callback2),

    ?assertEqual(nsime_node:destroy(NodePid), stopped),
    ?assertEqual(nsime_udp_echo_server:destroy(ServerPid), stopped).

test_start(_) ->
    ServerPid = nsime_udp_echo_server:create(),
    ?assert(is_pid(ServerPid)),
    ?assertEqual(nsime_udp_echo_server:set_listen_port(ServerPid, 9), ok),

    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),
    ?assertEqual(nsime_udp_echo_server:set_node(ServerPid, NodePid), ok),

    UdpProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(UdpProtocolPid)),
    ?assertEqual(nsime_node:add_object(NodePid, nsime_udp_protocol, UdpProtocolPid), ok),
    ?assertEqual(nsime_udp_protocol:set_node(UdpProtocolPid, NodePid), ok),

    ?assertEqual(nsime_udp_echo_server:start(ServerPid), ok),
    ?assertEqual(nsime_udp_echo_server:start(ServerPid), ok),
    nsime_simulator:start(),
    ?assertEqual(nsime_udp_echo_server:schedule_start(ServerPid, {0, sec}), ok),
    ?assertEqual(nsime_udp_echo_server:schedule_stop(ServerPid, {10, sec}), ok),
    ?assertEqual(nsime_udp_echo_server:stop(ServerPid), ok),

    ?assertEqual(nsime_simulator:stop(), simulation_complete),
    ?assertEqual(nsime_udp_echo_server:destroy(ServerPid), stopped).

test_cast_info_codechange(_) ->
    ServerPid = nsime_udp_echo_server:create(),
    ?assert(is_pid(ServerPid)),
    gen_server:cast(ServerPid, junk),
    ServerPid ! junk,
    nsime_udp_echo_server:code_change(junk, junk, junk),
    ?assertEqual(nsime_udp_echo_server:destroy(ServerPid), stopped).
