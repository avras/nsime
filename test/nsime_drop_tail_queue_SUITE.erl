%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Test module for nsime_drop_tail_queue
%% Author : Saravanan Vijayakumaran

-module(nsime_drop_tail_queue_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_dtq_state.hrl").

all() -> [
            test_creation_shutdown,
            test_creation_with_initial_state,
            test_enqueue_dequeue,
            test_drop_packet,
            test_dequeue_all_packets,
            test_reset_statistics,
            test_set_get_device_id
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
   QueuePid = nsime_drop_tail_queue:create(),
    case is_pid(QueuePid) of
        false ->
            ct:fail("Failed to create nsime_drop_tail_queue process",[]);
        true ->
            ?assert(nsime_drop_tail_queue:is_empty(QueuePid)),
            QueueStats = nsime_drop_tail_queue:get_statistics(QueuePid),
            ?assertEqual(QueueStats#nsime_dtq_state.current_packet_count, 0),
            ?assertEqual(QueueStats#nsime_dtq_state.current_byte_count, 0),
            ?assertEqual(QueueStats#nsime_dtq_state.received_packet_count, 0),
            ?assertEqual(QueueStats#nsime_dtq_state.received_byte_count, 0),
            ?assertEqual(QueueStats#nsime_dtq_state.dropped_packet_count, 0),
            ?assertEqual(QueueStats#nsime_dtq_state.dropped_byte_count, 0),
            ?assertEqual(QueueStats#nsime_dtq_state.max_packet_count, infinity),
            ?assertEqual(QueueStats#nsime_dtq_state.max_byte_count, infinity),
            ?assertEqual(QueueStats#nsime_dtq_state.device_id, undefined),
            ?assert(queue:is_empty(QueueStats#nsime_dtq_state.packets)),
            ?assertEqual(nsime_drop_tail_queue:destroy(QueuePid), stopped)
    end.

test_creation_with_initial_state(_) ->
    MaxPackets = 2,
    QueueState = #nsime_dtq_state{max_packet_count = MaxPackets},
    QueuePid = nsime_drop_tail_queue:create(QueueState),
    QueueStats = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats#nsime_dtq_state.current_packet_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.current_byte_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.received_packet_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.received_byte_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.max_packet_count, 2),
    ?assertEqual(QueueStats#nsime_dtq_state.max_byte_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.device_id, undefined),
    ?assert(queue:is_empty(QueueStats#nsime_dtq_state.packets)),
    ?assertEqual(nsime_drop_tail_queue:destroy(QueuePid), stopped).


test_enqueue_dequeue(_) ->
    MaxPackets = 2,
    MaxBytes = 2000,
    QueueState = #nsime_dtq_state{max_packet_count = MaxPackets, max_byte_count = MaxBytes},
    QueuePid = nsime_drop_tail_queue:create(QueueState),
    ?assert(nsime_drop_tail_queue:is_empty(QueuePid)),
   
    Size = 1000,
    Packet1 = create_packet(Size),
    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet1), ok),
    ?assertNot(nsime_drop_tail_queue:is_empty(QueuePid)),
    QueueStats1 = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats1#nsime_dtq_state.current_packet_count, 1),
    ?assertEqual(QueueStats1#nsime_dtq_state.current_byte_count, Size),
    ?assertEqual(QueueStats1#nsime_dtq_state.received_packet_count, 1),
    ?assertEqual(QueueStats1#nsime_dtq_state.received_byte_count, Size),
    ?assertEqual(QueueStats1#nsime_dtq_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats1#nsime_dtq_state.max_byte_count, MaxBytes),


    Packet2 = create_packet(1000),
    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet2), ok),
    ?assertNot(nsime_drop_tail_queue:is_empty(QueuePid)),
    QueueStats2 = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats2#nsime_dtq_state.current_packet_count, 2),
    ?assertEqual(QueueStats2#nsime_dtq_state.current_byte_count, 2*Size),
    ?assertEqual(QueueStats2#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats2#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats2#nsime_dtq_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats2#nsime_dtq_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats2#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats2#nsime_dtq_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, create_packet(1000)), dropped),
    ?assertNot(nsime_drop_tail_queue:is_empty(QueuePid)),
    QueueStats3 = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats3#nsime_dtq_state.current_packet_count, 2),
    ?assertEqual(QueueStats3#nsime_dtq_state.current_byte_count, 2*Size),
    ?assertEqual(QueueStats3#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats3#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats3#nsime_dtq_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats3#nsime_dtq_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats3#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats3#nsime_dtq_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_drop_tail_queue:dequeue_packet(QueuePid), Packet1),
    ?assertNot(nsime_drop_tail_queue:is_empty(QueuePid)),
    QueueStats4 = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats4#nsime_dtq_state.current_packet_count, 1),
    ?assertEqual(QueueStats4#nsime_dtq_state.current_byte_count, Size),
    ?assertEqual(QueueStats4#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats4#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats4#nsime_dtq_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats4#nsime_dtq_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats4#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats4#nsime_dtq_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_drop_tail_queue:dequeue_packet(QueuePid), Packet2),
    ?assert(nsime_drop_tail_queue:is_empty(QueuePid)),
    QueueStats5 = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats5#nsime_dtq_state.current_packet_count, 0),
    ?assertEqual(QueueStats5#nsime_dtq_state.current_byte_count, 0),
    ?assertEqual(QueueStats5#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats5#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats5#nsime_dtq_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats5#nsime_dtq_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats5#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats5#nsime_dtq_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_drop_tail_queue:dequeue_packet(QueuePid), none),
    ?assert(nsime_drop_tail_queue:is_empty(QueuePid)),
    QueueStats6 = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats6#nsime_dtq_state.current_packet_count, 0),
    ?assertEqual(QueueStats6#nsime_dtq_state.current_byte_count, 0),
    ?assertEqual(QueueStats6#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats6#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats6#nsime_dtq_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats6#nsime_dtq_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats6#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats6#nsime_dtq_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_drop_tail_queue:destroy(QueuePid), stopped).

test_drop_packet(_) ->
    MaxPackets = 3,
    MaxBytes = 3000,
    QueueState = #nsime_dtq_state{max_packet_count = MaxPackets, max_byte_count = MaxBytes},
    QueuePid = nsime_drop_tail_queue:create(QueueState),
    ?assert(is_pid(QueuePid)),
    Size = 1000,
    Packet1 = create_packet(Size),
    PacketId1 = Packet1#nsime_packet.id,
    Packet2 = create_packet(Size),
    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet1), ok),
    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet2), ok),

    ?assertEqual(nsime_drop_tail_queue:drop_packet(QueuePid, PacketId1), ok),
    QueueStats = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats#nsime_dtq_state.current_packet_count, 1),
    ?assertEqual(QueueStats#nsime_dtq_state.current_byte_count, Size),
    ?assertEqual(QueueStats#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats#nsime_dtq_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_drop_tail_queue:drop_packet(QueuePid, PacketId1), none),
    ?assertEqual(QueueStats#nsime_dtq_state.current_packet_count, 1),
    ?assertEqual(QueueStats#nsime_dtq_state.current_byte_count, Size),
    ?assertEqual(QueueStats#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats#nsime_dtq_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet1), ok),
    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet1), ok),
    ?assertEqual(nsime_drop_tail_queue:drop_packet(QueuePid, PacketId1), invalid_state),
    ?assertEqual(nsime_drop_tail_queue:dequeue_packet(QueuePid), Packet2),
    ?assertEqual(nsime_drop_tail_queue:dequeue_packet(QueuePid), Packet1),
    ?assertEqual(nsime_drop_tail_queue:dequeue_packet(QueuePid), Packet1),
    ?assertEqual(nsime_drop_tail_queue:destroy(QueuePid), stopped).

test_dequeue_all_packets(_) ->
    MaxPackets = 3,
    MaxBytes = 3000,
    QueueState = #nsime_dtq_state{max_packet_count = MaxPackets, max_byte_count = MaxBytes},
    QueuePid = nsime_drop_tail_queue:create(QueueState),
    ?assert(is_pid(QueuePid)),
    Size = 1000,
    Packet1 = create_packet(Size),
    Packet2 = create_packet(Size),
    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet1), ok),
    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet2), ok),

    QueueStats = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats#nsime_dtq_state.current_packet_count, 2),
    ?assertEqual(QueueStats#nsime_dtq_state.current_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats#nsime_dtq_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_drop_tail_queue:dequeue_all_packets(QueuePid), ok),
    ?assert(nsime_drop_tail_queue:is_empty(QueuePid)),
    QueueStats1 = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats1#nsime_dtq_state.current_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.current_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats1#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats1#nsime_dtq_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats1#nsime_dtq_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_drop_tail_queue:destroy(QueuePid), stopped).

test_reset_statistics(_) ->
    MaxPackets = 3,
    MaxBytes = 3000,
    QueueState = #nsime_dtq_state{max_packet_count = MaxPackets, max_byte_count = MaxBytes},
    QueuePid = nsime_drop_tail_queue:create(QueueState),
    ?assert(is_pid(QueuePid)),
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    ?assertEqual(nsime_drop_tail_queue:set_device_id(QueuePid, DevicePid), ok),

    Size = 1000,
    Packet1 = create_packet(Size),
    Packet2 = create_packet(Size),
    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet1), ok),
    ?assertEqual(nsime_drop_tail_queue:enqueue_packet(QueuePid, Packet2), ok),

    QueueStats = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats#nsime_dtq_state.current_packet_count, 2),
    ?assertEqual(QueueStats#nsime_dtq_state.current_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_dtq_state.received_packet_count, 2),
    ?assertEqual(QueueStats#nsime_dtq_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats#nsime_dtq_state.max_byte_count, MaxBytes),
    ?assertEqual(QueueStats#nsime_dtq_state.device_id, DevicePid),

    ?assertEqual(nsime_drop_tail_queue:reset_statistics(QueuePid), ok),
    ?assertNot(nsime_drop_tail_queue:is_empty(QueuePid)),
    QueueStats1 = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats1#nsime_dtq_state.current_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.current_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.received_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.received_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_dtq_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats1#nsime_dtq_state.max_byte_count, MaxBytes),
    ?assertEqual(QueueStats#nsime_dtq_state.device_id, DevicePid),

    ?assertEqual(nsime_drop_tail_queue:dequeue_packet(QueuePid), Packet1),
    ?assertEqual(nsime_drop_tail_queue:dequeue_packet(QueuePid), Packet2),

    ?assertEqual(nsime_drop_tail_queue:destroy(QueuePid), stopped),
    ?assertEqual(nsime_ptp_netdevice:destroy(DevicePid), killed).



test_set_get_device_id(_) ->
    QueuePid = nsime_drop_tail_queue:create(),
    ?assert(is_pid(QueuePid)),
    DevicePid = nsime_ptp_netdevice:create(),
    ?assert(is_pid(DevicePid)),
    ?assertEqual(nsime_drop_tail_queue:set_device_id(QueuePid, DevicePid), ok),
    QueueStats = nsime_drop_tail_queue:get_statistics(QueuePid),
    ?assertEqual(QueueStats#nsime_dtq_state.device_id, DevicePid),
    ?assertEqual(nsime_drop_tail_queue:get_device_id(QueuePid), DevicePid),
    nsime_ptp_netdevice:destroy(DevicePid).

create_packet(Size) ->
    #nsime_packet{
        id = make_ref(),
        size = Size
    }.
