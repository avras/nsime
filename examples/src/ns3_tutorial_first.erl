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

%% Purpose : nsime port of ns3/examples/tutorial/first.cc
%% Author : Saravanan Vijayakumaran

-module(ns3_tutorial_first).
-author("Saravanan Vijayakumaran").

-export([run/0]).

run() ->
    nsime_simulator:start(),
    NodePidList = nsime_node:create(2),
    [Node1, Node2] = NodePidList,

    PtpHelperPid = nsime_ptp_helper:create(),
    nsime_ptp_helper:call_on_device(PtpHelperPid, set_data_rate, {5, mega_bits_per_sec}),
    nsime_ptp_helper:call_on_channel(PtpHelperPid, set_channel_delay, {2, milli_sec}),
    DevicePidList = nsime_ptp_helper:install(PtpHelperPid, NodePidList),

    nsime_internet_stack_helper:install(NodePidList),

    AddressHelperPid = nsime_ipv4_address_helper:create(),
    nsime_ipv4_address_helper:set_base(AddressHelperPid, "10.1.1.0", "255.255.255.0"),
    InterfacePidList = nsime_ipv4_address_helper:assign(AddressHelperPid, DevicePidList),

    UdpEchoServerPid = nsime_udp_echo_server:create(),
    nsime_udp_echo_server:set_listen_port(UdpEchoServerPid, 9),
    nsime_node:add_application(Node2, UdpEchoServerPid),
    nsime_udp_echo_server:schedule_start(UdpEchoServerPid, {0, sec}),

    [_Interface1, Interface2] = InterfacePidList,
    UdpEchoClientPid = nsime_udp_echo_client:create(),
    RemoteAddress = nsime_ipv4_interface_address:get_local_address(
        hd(nsime_ipv4_interface:get_address_list(Interface2))
    ),
    nsime_udp_echo_client:set_remote(UdpEchoClientPid, RemoteAddress, 9),
    nsime_udp_echo_client:set_max_packets(UdpEchoClientPid, 1),
    nsime_udp_echo_client:set_inter_packet_gap(UdpEchoClientPid, {1, sec}),
    nsime_udp_echo_client:set_data_size(UdpEchoClientPid, 100),
    nsime_node:add_application(Node1, UdpEchoClientPid),
    nsime_udp_echo_client:schedule_start(UdpEchoClientPid, {2, sec}),

    nsime_simulator:run(),
    nsime_ptp_helper:destroy(PtpHelperPid),
    nsime_simulator:stop().
