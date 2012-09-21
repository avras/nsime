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

%% Purpose : Test module for nsime_node
%% Author : Saravanan Vijayakumaran

-module(nsime_node_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include("nsime_types.hrl").
-include("nsime_node_state.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [
          {group, testgroup_all}
         ].

groups() ->
    [{
        testgroup_all,
        [parallel],
        [
          test_creation_shutdown,
          test_creation_multiple_nodes,
          test_add_netdevice,
          test_add_application,
          test_cast_info_codechange
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

test_creation_shutdown(_) ->
    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)), 
    ?assertMatch([], nsime_node:get_netdevices(NodePid)),
    ?assertMatch([], nsime_node:get_applications(NodePid)),
    ?assertEqual(nsime_node:destroy(NodePid), stopped).

test_creation_multiple_nodes(_) ->
    N = 10,
    NodePidList = nsime_node:create(N),
    ?assert(lists:map(fun(X) -> is_pid(X) end, NodePidList) 
            =:= lists:duplicate(N, true)),
    lists:map(
        fun(Pid) ->
            ?assertMatch([], nsime_node:get_netdevices(Pid)),
            ?assertMatch([], nsime_node:get_applications(Pid)),
            ?assertEqual(nsime_node:destroy(Pid), stopped)
            end,
        NodePidList
    ).

test_add_netdevice(_) ->
    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    ?assertEqual(nsime_node:add_netdevice(NodePid, DevicePid), ok),
    [DevicePid] = nsime_node:get_netdevices(NodePid),
    ?assertEqual(nsime_node:destroy(NodePid), stopped).

test_add_application(_) ->
    nsime_simulator:start(),
    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),
    AppPid = nsime_udp_echo_server:create(),
    ?assert(is_pid(AppPid)),
    ?assertEqual(nsime_node:add_application(NodePid, AppPid), ok),
    [AppPid] = nsime_node:get_applications(NodePid),
    ?assertEqual(nsime_node:destroy(NodePid), stopped),
    ?assertEqual(nsime_simulator:stop(), stopped).

test_cast_info_codechange(_) ->
    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),
    gen_server:cast(NodePid, junk),
    NodePid ! junk,
    nsime_node:code_change(junk, junk, junk),
    ?assertEqual(nsime_node:destroy(NodePid), stopped).
