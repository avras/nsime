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

%% Purpose : Creation of multiple UDP client server pairs
%% Author : Saravanan Vijayakumaran

-module(udp_cs_pairs).
-author("Saravanan Vijayakumaran").

-export([start/0]).

start() ->
    nsime_simulator:start(),
    NumPairs = 10000,
    NodePidList = plists:map(
        fun(_) -> nsime_node:create() end,
        lists:seq(1, 2*NumPairs),
        {processes, 4}
    ),
    {ClientPids, ServerPids} = lists:split(NumPairs, NodePidList),
    ClientServerTuples = lists:zip(ClientPids, ServerPids),

    PtpHelperPid = nsime_ptp_helper:create(),
    nsime_ptp_helper:call_on_device(PtpHelperPid, set_data_rate, {5, mega_bits_per_sec}),
    nsime_ptp_helper:call_on_channel(PtpHelperPid, set_channel_delay, {2, milli_sec}),

    nsime_internet_stack_helper:install(NodePidList),

    AddressHelperPid = nsime_ipv4_address_helper:create(),
    nsime_ipv4_address_helper:set_base(AddressHelperPid, "10.0.0.0", "255.0.0.0"),

    lists:foreach(
        fun({ClientNode, ServerNode}) ->
            DevicePidList = nsime_ptp_helper:install(PtpHelperPid, [ClientNode, ServerNode]),
            [_Interface1, Interface2] = nsime_ipv4_address_helper:assign(AddressHelperPid, DevicePidList),

            UdpEchoServerPid = nsime_udp_echo_server:create(),
            nsime_udp_echo_server:set_listen_port(UdpEchoServerPid, 9),
            nsime_node:add_application(ServerNode, UdpEchoServerPid),
            nsime_udp_echo_server:schedule_start(UdpEchoServerPid, {1, sec}),
            nsime_udp_echo_server:schedule_stop(UdpEchoServerPid, {3, sec}),

            UdpEchoClientPid = nsime_udp_echo_client:create(),
            RemoteAddress = nsime_ipv4_interface_address:get_local_address(
                hd(nsime_ipv4_interface:get_address_list(Interface2))
            ),

            nsime_udp_echo_client:set_remote(UdpEchoClientPid, RemoteAddress, 9),
            nsime_udp_echo_client:set_max_packets(UdpEchoClientPid, 10000),
            nsime_udp_echo_client:set_inter_packet_gap(UdpEchoClientPid, {0.1, sec}),
            nsime_udp_echo_client:set_data_size(UdpEchoClientPid, 1024),
            nsime_node:add_application(ClientNode, UdpEchoClientPid),
            nsime_udp_echo_client:schedule_start(UdpEchoClientPid, {2, sec}),
            nsime_udp_echo_client:schedule_stop(UdpEchoClientPid, {3, sec})
        end,
        ClientServerTuples
    ),


    nsime_simulator:parallel_run(),
    nsime_ptp_helper:destroy(PtpHelperPid),
    nsime_ipv4_address_helper:destroy(AddressHelperPid),
    nsime_simulator:stop().
