%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
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
            test_attach_channel
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
    ?assertEqual(nsime_ptp_netdevice:get_queue(DevicePid), undefined),
    ?assertNot(nsime_ptp_netdevice:is_link_up(DevicePid)),
    ?assertEqual(nsime_ptp_netdevice:get_mtu(DevicePid), 1500),
    ?assertEqual(nsime_ptp_netdevice:get_device_index(DevicePid), undefined),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid), killed),

    DeviceState = #nsime_ptp_netdevice_state{},
    DevicePid1 = nsime_ptp_netdevice:create(DeviceState),
    ?assert(is_pid(DevicePid1)),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid1), killed).

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
    ?assertMatch({EtherProtNumber, Packet}, nsime_ptp_netdevice:process_ppp_header(NewPacket)),
    ok.

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
    ?assertEqual(nsime_ptp_netdevice:attach_channel(DevicePid3, ChannelPid), none),
    ok.


create_packet(Id, Size, Data) ->
    #nsime_packet{
        id = Id,
        size = Size,
        data = Data
    }.
