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

%% Purpose : Test module for nsime_udp_echo_client
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_echo_client_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_udp_echo_client_state.hrl").

all() -> [
            test_creation_shutdown,
            test_set_get_components,
            test_start,
            test_send,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    ClientPid = nsime_udp_echo_client:create(),
    ?assert(is_pid(ClientPid)),
    ?assertEqual(nsime_udp_echo_client:destroy(ClientPid), stopped).

test_set_get_components(_) ->
    ClientPid = nsime_udp_echo_client:create(),
    ?assert(is_pid(ClientPid)),
    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),

    ?assertEqual(nsime_udp_echo_client:set_node(ClientPid, NodePid), ok),
    ?assertEqual(nsime_udp_echo_client:get_node(ClientPid), NodePid),
    Address = {10, 107, 1, 1},
    Port = 80,
    ?assertEqual(nsime_udp_echo_client:set_remote(ClientPid, Address, Port), ok),
    ?assertError(invalid_argument, nsime_udp_echo_client:set_remote(ClientPid, Address, -1)),
    ?assertError(invalid_argument, nsime_udp_echo_client:set_remote(ClientPid, Address, 65536)),
    ?assertError(invalid_argument, nsime_udp_echo_client:set_remote(ClientPid, tuple_to_list(Address), 65536)),

    DataSize = 1500,
    ?assertEqual(nsime_udp_echo_client:set_data_size(ClientPid, DataSize), ok),
    ?assertEqual(nsime_udp_echo_client:get_data_size(ClientPid), DataSize),
    MaxPackets = 100,
    ?assertEqual(nsime_udp_echo_client:set_max_packets(ClientPid, MaxPackets), ok),
    ?assertEqual(nsime_udp_echo_client:get_max_packets(ClientPid), MaxPackets),
    InterPacketGap = {1, sec},
    ?assertEqual(nsime_udp_echo_client:set_inter_packet_gap(ClientPid, InterPacketGap), ok),
    ?assertEqual(nsime_udp_echo_client:get_inter_packet_gap(ClientPid), InterPacketGap),

    Callback1 = {erlang, date, [1]},
    ?assertEqual(nsime_udp_echo_client:set_transmit_trace_callback(ClientPid, Callback1), ok),
    ?assertEqual(nsime_udp_echo_client:get_transmit_trace_callback(ClientPid), Callback1),
    Callback2 = {erlang, date, [2]},
    ?assertEqual(nsime_udp_echo_client:set_receive_trace_callback(ClientPid, Callback2), ok),
    ?assertEqual(nsime_udp_echo_client:get_receive_trace_callback(ClientPid), Callback2),

    ?assertEqual(nsime_node:destroy(NodePid), stopped),
    ?assertEqual(nsime_udp_echo_client:destroy(ClientPid), stopped).

test_start(_) ->
    ClientPid = nsime_udp_echo_client:create(),
    ?assert(is_pid(ClientPid)),
    Address = {10, 107, 1, 1},
    Port = 80,
    ?assertEqual(nsime_udp_echo_client:set_remote(ClientPid, Address, Port), ok),

    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),
    ?assertEqual(nsime_udp_echo_client:set_node(ClientPid, NodePid), ok),

    UdpProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(UdpProtocolPid)),
    ?assertEqual(nsime_node:add_object(NodePid, nsime_udp_protocol, UdpProtocolPid), ok),
    ?assertEqual(nsime_udp_protocol:set_node(UdpProtocolPid, NodePid), ok),

    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),
    ?assertEqual(nsime_udp_echo_client:start(ClientPid), ok),
    ?assertEqual(nsime_udp_echo_client:start(ClientPid), ok),
    ?assertEqual(nsime_udp_echo_client:schedule_start(ClientPid, {0, sec}), ok),
    ?assertEqual(nsime_udp_echo_client:stop(ClientPid), ok),

    ?assertEqual(nsime_simulator:stop(), simulation_complete),
    ?assertEqual(nsime_udp_echo_client:destroy(ClientPid), stopped).

test_send(_) ->
    ClientPid = nsime_udp_echo_client:create(),
    ?assert(is_pid(ClientPid)),
    DataSize = 1500,
    Address = {10, 107, 1, 1},
    Port = 80,
    ?assertEqual(nsime_udp_echo_client:set_remote(ClientPid, Address, Port), ok),

    NodePid = nsime_node:create(),
    ?assert(is_pid(NodePid)),
    ?assertEqual(nsime_udp_echo_client:set_node(ClientPid, NodePid), ok),

    UdpProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(UdpProtocolPid)),
    ?assertEqual(nsime_node:add_object(NodePid, nsime_udp_protocol, UdpProtocolPid), ok),
    ?assertEqual(nsime_udp_protocol:set_node(UdpProtocolPid, NodePid), ok),

    Ipv4ProtocolPid = nsime_ipv4_protocol:create(),
    ?assert(is_pid(Ipv4ProtocolPid)),
    ?assertEqual(nsime_node:add_object(NodePid, nsime_ipv4_protocol, Ipv4ProtocolPid), ok),
    ?assertEqual(nsime_ipv4_protocol:set_node(Ipv4ProtocolPid, NodePid), ok),

    nsime_simulator:start(),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assert(lists:member(nsime_gbtrees_scheduler, erlang:registered())),
    ?assertEqual(nsime_udp_echo_client:start(ClientPid), ok),
    Ref = make_ref(),
    TransmitCallback = {
        ?MODULE,
        transmit_trace_tester,
        [self(), Ref]
    },
    ?assertEqual(nsime_udp_echo_client:set_transmit_trace_callback(ClientPid, TransmitCallback), ok),
    ?assertEqual(nsime_udp_echo_client:send(ClientPid), ok),
    receive
        {transmit_trace, Ref} ->
            ok
    end,

    ?assertEqual(nsime_udp_echo_client:destroy(ClientPid), stopped).

test_cast_info_codechange(_) ->
    ClientPid = nsime_udp_echo_client:create(),
    ?assert(is_pid(ClientPid)),
    gen_server:cast(ClientPid, junk),
    ClientPid ! junk,
    nsime_udp_echo_client:code_change(junk, junk, junk),
    ?assertEqual(nsime_udp_echo_client:destroy(ClientPid), stopped).

%% Helper methods %%

transmit_trace_tester(From, Ref, _Packet) ->
    spawn(fun() -> From ! {transmit_trace, Ref} end).


