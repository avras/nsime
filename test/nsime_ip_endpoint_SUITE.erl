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

%% Purpose : Test module for nsime_ip_endpoint
%% Author : Saravanan Vijayakumaran

-module(nsime_ip_endpoint_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_ip_endpoint_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
            test_set_get_callbacks,
            test_forward,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    EndpointPid1 = nsime_ip_endpoint:create(),
    ?assert(is_pid(EndpointPid1)),
    ?assertEqual(nsime_ip_endpoint:destroy(EndpointPid1), stopped),
    EndpointState = #nsime_ip_endpoint_state{
        local_address = {10, 107, 1, 1},
        local_port = 8080,
        peer_address = {192, 168, 0, 1},
        peer_port = 80
    },
    EndpointPid2 = nsime_ip_endpoint:create(EndpointState),
    ?assert(is_pid(EndpointPid2)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid2),
        EndpointState#nsime_ip_endpoint_state.local_address
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointPid2),
        EndpointState#nsime_ip_endpoint_state.local_port
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_address(EndpointPid2),
        EndpointState#nsime_ip_endpoint_state.peer_address
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_port(EndpointPid2),
        EndpointState#nsime_ip_endpoint_state.peer_port
    ),
    ?assertEqual(nsime_ip_endpoint:destroy(EndpointPid2), stopped),
    Address = {10, 107, 1, 2},
    Port = 123,
    EndpointPid3 = nsime_ip_endpoint:create(Address, Port),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid3),
        Address
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointPid3),
        Port
    ),
    ?assertEqual(nsime_ip_endpoint:destroy(EndpointPid3), stopped).

test_set_get_components(_) ->
    EndpointPid = nsime_ip_endpoint:create(),
    ?assert(is_pid(EndpointPid)),
    DevicePid = list_to_pid("<0.1.1>"),
    ?assertEqual(nsime_ip_endpoint:bind_to_netdevice(EndpointPid, DevicePid), ok),
    ?assertEqual(nsime_ip_endpoint:get_bound_netdevice(EndpointPid), DevicePid),
    LocalAddress = {10, 107, 1, 1},
    ?assertEqual(nsime_ip_endpoint:set_local_address(EndpointPid, LocalAddress), ok),
    ?assertEqual(nsime_ip_endpoint:get_local_address(EndpointPid), LocalAddress),
    LocalPort = 1234,
    ?assertEqual(nsime_ip_endpoint:set_local_port(EndpointPid, LocalPort), ok),
    ?assertEqual(nsime_ip_endpoint:get_local_port(EndpointPid), LocalPort),
    PeerAddress = {10, 107, 1, 2},
    PeerPort = 456,
    ?assertEqual(nsime_ip_endpoint:set_peer(EndpointPid, PeerAddress, PeerPort), ok),
    ?assertEqual(nsime_ip_endpoint:get_peer_address(EndpointPid), PeerAddress),
    ?assertEqual(nsime_ip_endpoint:get_peer_port(EndpointPid), PeerPort),
    ?assertEqual(nsime_ip_endpoint:destroy(EndpointPid), stopped).

test_set_get_callbacks(_) ->
    EndpointPid = nsime_ip_endpoint:create(),
    ?assert(is_pid(EndpointPid)),
    Callback1 = {erlang, date, []},
    ?assertEqual(nsime_ip_endpoint:set_receive_callback(EndpointPid, Callback1), ok),
    Callback2 = {erlang, time, []},
    ?assertEqual(nsime_ip_endpoint:set_icmp_callback(EndpointPid, Callback2), ok),
    Callback3 = {erlang, universaltime, []},
    ?assertEqual(nsime_ip_endpoint:set_destroy_callback(EndpointPid, Callback3), ok),
    ?assertEqual(nsime_ip_endpoint:destroy(EndpointPid), stopped).

test_forward(_) ->
    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    EndpointPid = nsime_ip_endpoint:create(),
    ?assert(is_pid(EndpointPid)),
    Ref1 = make_ref(),
    Callback1 = {
        ?MODULE,
        send_receive_callback,
        [Ref1, self()]
    },
    ?assertEqual(nsime_ip_endpoint:set_receive_callback(EndpointPid, Callback1), ok),
    ?assertEqual(nsime_ip_endpoint:forward_up(EndpointPid, [], [], [], []), ok),
    ?assertEqual(nsime_simulator:run(), simulation_complete),
    receive
        {receive_callback, Ref1} ->
            ok
    end,
    Ref2 = make_ref(),
    Callback2 = {
        ?MODULE,
        send_icmp_callback,
        [Ref2, self()]
    },
    ?assertEqual(nsime_ip_endpoint:set_icmp_callback(EndpointPid, Callback2), ok),
    ?assertEqual(nsime_ip_endpoint:forward_icmp(EndpointPid, [], [], [], [], []), ok),
    ?assertEqual(nsime_simulator:run(), simulation_complete),
    receive
        {icmp_callback, Ref2} ->
            ok
    end,
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_cast_info_codechange(_) ->
    EndpointPid = nsime_ip_endpoint:create(),
    ?assert(is_pid(EndpointPid)),
    gen_server:cast(EndpointPid, junk),
    EndpointPid ! junk,
    nsime_ip_endpoint:code_change(junk, junk, junk),
    ?assertEqual(nsime_ip_endpoint:destroy(EndpointPid), stopped).

%% Helper methods %%
send_receive_callback(Ref, Pid) ->
    Pid ! {receive_callback, Ref}.

send_icmp_callback(Ref, Pid) ->
    Pid ! {icmp_callback, Ref}.
