%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Test module for nsime_node
%% Author : Saravanan Vijayakumaran

-module(nsime_node_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
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
          test_add_application
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
    ?assertEqual(nsime_node:get_netdevice_count(NodePid), 0),
    ?assertEqual(nsime_node:get_application_count(NodePid), 0),
    ?assertMatch([], nsime_node:get_netdevices(NodePid)),
    ?assertMatch([], nsime_node:get_applications(NodePid)),
    ?assertEqual(nsime_node:destroy(NodePid), killed).

test_creation_multiple_nodes(_) ->
    N = 10,
    NodePidList = nsime_node:create(N),
    ?assert(lists:map(fun(X) -> is_pid(X) end, NodePidList) 
            =:= lists:duplicate(N, true)),
    lists:map(
        fun(Pid) ->
            ?assertEqual(nsime_node:get_netdevice_count(Pid), 0),
            ?assertEqual(nsime_node:get_application_count(Pid), 0),
            ?assertMatch([], nsime_node:get_netdevices(Pid)),
            ?assertMatch([], nsime_node:get_applications(Pid)),
            ?assertEqual(nsime_node:destroy(Pid), killed)
            end,
        NodePidList
    ).

test_add_netdevice(_) ->
    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),
    ?assertEqual(nsime_node:get_netdevice_count(NodePid), 0),
    ?assertEqual(nsime_node:add_netdevice(NodePid, nsime_ptp_netdevice), ok),
    ?assertEqual(nsime_node:get_netdevice_count(NodePid), 1),
    [DevicePid] = nsime_node:get_netdevices(NodePid),
    ?assert(is_pid(DevicePid)).

test_add_application(_) ->
    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),
    ?assertEqual(nsime_node:get_application_count(NodePid), 0),
    ?assertEqual(nsime_node:add_application(NodePid, nsime_ptp_netdevice), ok),
    ?assertEqual(nsime_node:get_application_count(NodePid), 1),
    [AppPid] = nsime_node:get_applications(NodePid),
    ?assert(is_pid(AppPid)).
