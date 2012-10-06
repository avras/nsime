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

%% Purpose : Test module for nsime_application
%% Author : Saravanan Vijayakumaran

-module(nsime_application_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [
            test_creation_shutdown,
            test_start
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    ServerPid = nsime_udp_echo_server:create(),
    ?assert(is_pid(ServerPid)),
    ?assertEqual(nsime_application:destroy(ServerPid), stopped).

test_start(_) ->
    ServerPid = nsime_udp_echo_server:create(),
    ?assert(is_pid(ServerPid)),
    ?assertEqual(nsime_udp_echo_server:set_listen_port(ServerPid, 9), ok),

    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),
    ?assertEqual(nsime_application:set_node(ServerPid, NodePid), ok),
    ?assertEqual(nsime_application:get_node(ServerPid), NodePid),

    UdpProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(UdpProtocolPid)),
    ?assertEqual(nsime_node:add_object(NodePid, nsime_udp_protocol, UdpProtocolPid), ok),
    ?assertEqual(nsime_udp_protocol:set_node(UdpProtocolPid, NodePid), ok),

    ?assertEqual(nsime_application:start(ServerPid), ok),
    ?assertEqual(nsime_application:start(ServerPid), ok),
    nsime_simulator:start(),
    ?assertEqual(nsime_application:schedule_start(ServerPid, {0, sec}), ok),
    ?assertEqual(nsime_application:stop(ServerPid), ok),

    ?assertEqual(nsime_simulator:stop(), stopped),
    ?assertEqual(nsime_application:destroy(ServerPid), stopped).
