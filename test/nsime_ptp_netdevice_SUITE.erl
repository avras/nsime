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

%% Purpose : Test module for nsime_ptp_netdevice
%% Author : Saravanan Vijayakumaran

-module(nsime_ptp_netdevice_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_ptp_netdevice_state.hrl").

all() -> [
            test_creation_shutdown,
            test_ppp_ether,
            test_add_process_header,
            test_set_get_components,
            test_attach_channel,
            test_transmit_start,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    ?assertEqual(nsime_ptp_netdevice:get_node(DevicePid), undefined),
    ?assertEqual(nsime_ptp_netdevice:get_address(DevicePid), undefined),
    ?assertEqual(nsime_ptp_netdevice:get_channel(DevicePid), undefined),
    ?assertEqual(nsime_ptp_netdevice:get_queue_module(DevicePid), nsime_drop_tail_queue),
    ?assert(is_pid(nsime_ptp_netdevice:get_queue(DevicePid))),
    ?assertNot(nsime_ptp_netdevice:is_link_up(DevicePid)),
    ?assertEqual(nsime_ptp_netdevice:get_mtu(DevicePid), 1500),
    ?assertEqual(nsime_ptp_netdevice:get_device_index(DevicePid), undefined),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid), stopped),

    DeviceState = #nsime_ptp_netdevice_state{},
    DevicePid1 = nsime_ptp_netdevice:create(DeviceState),
    ?assert(is_pid(DevicePid1)),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid1), stopped).

test_ppp_ether(_) ->
    ?assertEqual(nsime_ptp_netdevice:ppp_to_ether(16#0021), 16#0800),
    ?assertEqual(nsime_ptp_netdevice:ppp_to_ether(16#0057), 16#86DD),
    ?assertError(undefined_protocol, nsime_ptp_netdevice:ppp_to_ether(16#FFFF)),

    ?assertEqual(nsime_ptp_netdevice:ether_to_ppp(16#0800), 16#0021),
    ?assertEqual(nsime_ptp_netdevice:ether_to_ppp(16#86DD), 16#0057),
    ?assertError(undefined_protocol, nsime_ptp_netdevice:ether_to_ppp(16#FFFF)).

test_add_process_header(_) ->
    PacketId = make_ref(),
    Data = <<16#FFFF:16>>,
    Packet = create_packet(PacketId, 2, Data),
    EtherProtNumber = 16#0800,
    PPPProtNumber = nsime_ptp_netdevice:ether_to_ppp(EtherProtNumber),
    NewData = <<PPPProtNumber:16, Data/binary>>,
    NewPacket = Packet#nsime_packet{data = NewData, size = 4},
    ?assertMatch(NewPacket, nsime_ptp_netdevice:add_ppp_header(Packet, EtherProtNumber)),
    ?assertMatch({EtherProtNumber, Packet}, nsime_ptp_netdevice:process_ppp_header(NewPacket)).

test_set_get_components(_) ->
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    NodePid = list_to_pid("<0.1.1>"),
    ?assertEqual(nsime_ptp_netdevice:set_node(DevicePid, NodePid), ok),
    ?assertEqual(nsime_ptp_netdevice:get_node(DevicePid), NodePid),
    Address = <<16#FFFFFFFFFFFF:48>>,
    ?assertEqual(nsime_ptp_netdevice:set_address(DevicePid, Address), ok),
    ?assertEqual(nsime_ptp_netdevice:get_address(DevicePid), Address),
    DataRate = {10, bits_per_sec},
    ?assertEqual(nsime_ptp_netdevice:set_data_rate(DevicePid, DataRate), ok),
    InterFrameGap = {10, micro_sec},
    ?assertEqual(nsime_ptp_netdevice:set_interframe_gap(DevicePid, InterFrameGap), ok),
    QueuePid = list_to_pid("<0.2.2>"),
    ?assertEqual(nsime_ptp_netdevice:set_queue(DevicePid, QueuePid), ok),
    ?assertEqual(nsime_ptp_netdevice:get_queue(DevicePid), QueuePid),
    QueueModule = some_queue,
    ?assertEqual(nsime_ptp_netdevice:set_queue_module(DevicePid, QueueModule), ok),
    ?assertEqual(nsime_ptp_netdevice:get_queue_module(DevicePid), QueueModule),
    ErrorModel = some_error_model,
    ?assertEqual(nsime_ptp_netdevice:set_receive_error_model(DevicePid, ErrorModel), ok),
    MTU = 1000,
    ?assertEqual(nsime_ptp_netdevice:set_mtu(DevicePid, MTU), ok),
    ?assertEqual(nsime_ptp_netdevice:get_mtu(DevicePid), MTU),
    DeviceIndex = 10,
    ?assertEqual(nsime_ptp_netdevice:set_device_index(DevicePid, DeviceIndex), ok),
    ?assertEqual(nsime_ptp_netdevice:get_device_index(DevicePid), DeviceIndex).

test_attach_channel(_) ->
    ChannelPid = nsime_ptp_channel:create(),
    ?assert(is_pid(ChannelPid)),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    DevicePid2 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid2)),
    DevicePid3 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid3)),
    ?assertEqual(nsime_ptp_netdevice:attach_channel(DevicePid1, ChannelPid), ok),
    ?assertEqual(nsime_ptp_netdevice:attach_channel(DevicePid2, ChannelPid), ok),
    ?assertEqual(nsime_ptp_netdevice:attach_channel(DevicePid3, ChannelPid), none).

test_transmit_start(_) ->
   ChannelPid = nsime_ptp_channel:create(),
    ?assert(is_pid(ChannelPid)),
    DevicePid1 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid1)),
    DevicePid2 = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid2)),
    ?assertEqual(nsime_ptp_netdevice:transmit_start(DevicePid1, #nsime_packet{}), false),
    ?assertEqual(nsime_ptp_netdevice:transmit_complete(DevicePid1), false),
    ?assertEqual(nsime_ptp_netdevice:attach_channel(DevicePid1, ChannelPid), ok),
    ?assertEqual(nsime_ptp_netdevice:attach_channel(DevicePid2, ChannelPid), ok),

    DataRate = {10, bits_per_sec},
    ?assertEqual(nsime_ptp_netdevice:set_data_rate(DevicePid1, DataRate), ok),
    InterFrameGap = {10, micro_sec},
    ?assertEqual(nsime_ptp_netdevice:set_interframe_gap(DevicePid1, InterFrameGap), ok),

    nsime_simulator:start(),
    Data = <<0:160>>,
    Packet = create_packet(make_ref(), 20, Data),
    ?assertEqual(nsime_ptp_netdevice:transmit_start(DevicePid1, Packet), true),
    ?assertEqual(nsime_simulator:run(), simulation_complete),

    ?assertEqual(nsime_ptp_netdevice:transmit_start(DevicePid1, Packet), true),
    ?assertEqual(nsime_ptp_netdevice:transmit_start(DevicePid1, Packet#nsime_packet{size = 25}), true),
    ?assertEqual(nsime_ptp_netdevice:send_packet(DevicePid1, Packet, 16#86DD), true),
    ?assertEqual(nsime_simulator:run(), simulation_complete),

    ?assertEqual(nsime_simulator:stop(), stopped),
    ?assertEqual(nsime_ptp_channel:destroy(ChannelPid), stopped),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid1), stopped),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid2), stopped).

test_cast_info_codechange(_) ->
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    gen_server:cast(DevicePid, junk),
    DevicePid ! junk,
    nsime_ptp_netdevice:code_change(junk, junk, junk),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid), stopped).

create_packet(Id, Size, Data) ->
    #nsime_packet{
        id = Id,
        size = Size,
        data = Data
    }.
