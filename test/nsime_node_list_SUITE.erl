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

%% Purpose : Test module for nsime_node_list
%% Author : Saravanan Vijayakumaran

-module(nsime_node_list_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [
            test_start_stop,
            test_add_delete,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_start_stop(_) ->
    nsime_node_list:start(),
    Pid = erlang:whereis(nsime_node_list),
    ?assert(erlang:is_pid(Pid)),
    ?assert(lists:member(nsime_node_list, erlang:registered())),
    ?assert(gb_sets:is_empty(nsime_node_list:get_node_list())),
    ?assertEqual(nsime_node_list:stop(), stopped),
    ?assertNot(lists:member(nsime_node_list, erlang:registered())).

test_add_delete(_) ->
    NodePid1 = nsime_node:create(),
    ?assert(erlang:is_pid(NodePid1)),
    NodePid2 = nsime_node:create(),
    ?assert(erlang:is_pid(NodePid2)),
    nsime_node_list:start(),
    ?assertEqual(nsime_node_list:add(NodePid1), ok),
    ?assertEqual(nsime_node_list:add(NodePid2), ok),
    DeviceList = nsime_node_list:get_node_list(),
    ?assert(gb_sets:is_member(NodePid1, DeviceList)),
    ?assert(gb_sets:is_member(NodePid2, DeviceList)),
    
    ?assertEqual(nsime_node_list:delete(NodePid1), ok),
    ?assertEqual(nsime_node_list:delete(NodePid1), none),
    DeviceList1 = nsime_node_list:get_node_list(),
    ?assertNot(gb_sets:is_member(NodePid1, DeviceList1)),
    ?assert(gb_sets:is_member(NodePid2, DeviceList1)),
    ?assertEqual(nsime_node:destroy(NodePid1), stopped),
    ?assertEqual(nsime_node:destroy(NodePid2), stopped),
    ?assertEqual(nsime_node_list:stop(), stopped).

test_cast_info_codechange(_) ->
    nsime_node_list:start(),
    Pid = erlang:whereis(nsime_node_list),
    ?assert(erlang:is_pid(Pid)),
    gen_server:cast(nsime_node_list, junk),
    Pid ! junk,
    nsime_node_list:code_change(junk, junk, junk),
    ?assertEqual(nsime_node_list:stop(), stopped).
