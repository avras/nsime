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
            test_forward
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    EndpointState1 = nsime_ip_endpoint:create(),
    ?assert(is_record(EndpointState1, nsime_ip_endpoint_state)),
    Address = {10, 107, 1, 2},
    Port = 123,
    Callbacks = {{m1, f1, a1}, {m2, f2, a2}, {m3, f3, a3}},
    EndpointState2 = nsime_ip_endpoint:create(Address, Port, Callbacks),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState2),
        Address
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointState2),
        Port
    ).

test_set_get_components(_) ->
    EndpointState = nsime_ip_endpoint:create(),
    ?assert(is_record(EndpointState, nsime_ip_endpoint_state)),
    DevicePid = list_to_pid("<0.1.1>"),
    EndpointState1 = nsime_ip_endpoint:bind_to_netdevice(EndpointState, DevicePid),
    ?assertEqual(nsime_ip_endpoint:get_bound_netdevice(EndpointState1), DevicePid),
    LocalAddress = {10, 107, 1, 1},
    EndpointState2 = nsime_ip_endpoint:set_local_address(EndpointState1, LocalAddress),
    ?assertEqual(nsime_ip_endpoint:get_local_address(EndpointState2), LocalAddress),
    LocalPort = 1234,
    EndpointState3 = nsime_ip_endpoint:set_local_port(EndpointState2, LocalPort),
    ?assertEqual(nsime_ip_endpoint:get_local_port(EndpointState3), LocalPort),
    PeerAddress = {10, 107, 1, 2},
    PeerPort = 456,
    EndpointState4 = nsime_ip_endpoint:set_peer(EndpointState3, PeerAddress, PeerPort),
    ?assertEqual(nsime_ip_endpoint:get_peer_address(EndpointState4), PeerAddress),
    ?assertEqual(nsime_ip_endpoint:get_peer_port(EndpointState4), PeerPort).

test_set_get_callbacks(_) ->
    EndpointState = nsime_ip_endpoint:create(),
    ?assert(is_record(EndpointState, nsime_ip_endpoint_state)),
    Callback1 = {erlang, date, []},
    EndpointState1 = nsime_ip_endpoint:set_receive_callback(EndpointState, Callback1),
    ?assert(is_record(EndpointState1, nsime_ip_endpoint_state)),
    Callback2 = {erlang, time, []},
    EndpointState2 = nsime_ip_endpoint:set_icmp_callback(EndpointState1, Callback2),
    ?assert(is_record(EndpointState2, nsime_ip_endpoint_state)),
    Callback3 = {erlang, universaltime, []},
    EndpointState3 = nsime_ip_endpoint:set_destroy_callback(EndpointState2, Callback3),
    ?assert(is_record(EndpointState3, nsime_ip_endpoint_state)).

test_forward(_) ->
    EndpointState = nsime_ip_endpoint:create(),
    ?assert(is_record(EndpointState, nsime_ip_endpoint_state)),
    Ref1 = make_ref(),
    Callback1 = {
        ?MODULE,
        send_receive_callback,
        [Ref1, self()]
    },
    EndpointState1 = nsime_ip_endpoint:set_receive_callback(EndpointState, Callback1),
    nsime_ip_endpoint:forward_up(EndpointState1, [], [], [], []),
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
    EndpointState2 = nsime_ip_endpoint:set_icmp_callback(EndpointState1, Callback2),
    nsime_ip_endpoint:forward_icmp(EndpointState2, [], [], [], [], []),
    receive
        {icmp_callback, Ref2} ->
            ok
    end.

%% Helper methods %%
send_receive_callback(Ref, Pid) ->
    Pid ! {receive_callback, Ref}.

send_icmp_callback(Ref, Pid) ->
    Pid ! {icmp_callback, Ref}.
