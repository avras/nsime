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

%% Purpose : Test module for nsime_droptail_queue
%% Author : Saravanan Vijayakumaran

-module(nsime_droptail_queue_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_droptail_queue_state.hrl").

all() -> [
            test_creation,
            test_enqueue_dequeue,
            test_drop_packet,
            test_dequeue_all_packets,
            test_reset_statistics
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation(_) ->
    QueueState = nsime_droptail_queue:create(),
    ?assert(is_record(QueueState, nsime_droptail_queue_state)),
    ?assert(nsime_droptail_queue:is_empty(QueueState)),
    QueueStats = nsime_droptail_queue:get_statistics(QueueState),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.current_packet_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.current_byte_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.received_packet_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.received_byte_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.max_packet_count, infinity),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.max_byte_count, infinity),
    ?assert(queue:is_empty(QueueStats#nsime_droptail_queue_state.packets)).

test_enqueue_dequeue(_) ->
    MaxPackets = 2,
    MaxBytes = 2000,
    QueueState = #nsime_droptail_queue_state{max_packet_count = MaxPackets, max_byte_count = MaxBytes},
    ?assert(nsime_droptail_queue:is_empty(QueueState)),
   
    Size = 1000,
    Packet1 = create_packet(Size),
    QueueState1 = nsime_droptail_queue:enqueue_packet(QueueState, Packet1),
    ?assert(is_record(QueueState1, nsime_droptail_queue_state)),
    ?assertNot(nsime_droptail_queue:is_empty(QueueState1)),
    QueueStats1 = nsime_droptail_queue:get_statistics(QueueState1),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.current_packet_count, 1),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.current_byte_count, Size),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.received_packet_count, 1),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.received_byte_count, Size),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.max_byte_count, MaxBytes),


    Packet2 = create_packet(1000),
    QueueState2 = nsime_droptail_queue:enqueue_packet(QueueState1, Packet2),
    ?assert(is_record(QueueState2, nsime_droptail_queue_state)),
    ?assertNot(nsime_droptail_queue:is_empty(QueueState2)),
    QueueStats2 = nsime_droptail_queue:get_statistics(QueueState2),
    ?assertEqual(QueueStats2#nsime_droptail_queue_state.current_packet_count, 2),
    ?assertEqual(QueueStats2#nsime_droptail_queue_state.current_byte_count, 2*Size),
    ?assertEqual(QueueStats2#nsime_droptail_queue_state.received_packet_count, 2),
    ?assertEqual(QueueStats2#nsime_droptail_queue_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats2#nsime_droptail_queue_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats2#nsime_droptail_queue_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats2#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats2#nsime_droptail_queue_state.max_byte_count, MaxBytes),

    QueueState3 = nsime_droptail_queue:enqueue_packet(QueueState2, create_packet(1000)),
    ?assert(is_record(QueueState3, nsime_droptail_queue_state)),
    ?assertNot(nsime_droptail_queue:is_empty(QueueState3)),
    QueueStats3 = nsime_droptail_queue:get_statistics(QueueState3),
    ?assertEqual(QueueStats3#nsime_droptail_queue_state.current_packet_count, 2),
    ?assertEqual(QueueStats3#nsime_droptail_queue_state.current_byte_count, 2*Size),
    ?assertEqual(QueueStats3#nsime_droptail_queue_state.received_packet_count, 2),
    ?assertEqual(QueueStats3#nsime_droptail_queue_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats3#nsime_droptail_queue_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats3#nsime_droptail_queue_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats3#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats3#nsime_droptail_queue_state.max_byte_count, MaxBytes),

    {Packet1, QueueState4} = nsime_droptail_queue:dequeue_packet(QueueState3),
    ?assertNot(nsime_droptail_queue:is_empty(QueueState4)),
    QueueStats4 = nsime_droptail_queue:get_statistics(QueueState4),
    ?assertEqual(QueueStats4#nsime_droptail_queue_state.current_packet_count, 1),
    ?assertEqual(QueueStats4#nsime_droptail_queue_state.current_byte_count, Size),
    ?assertEqual(QueueStats4#nsime_droptail_queue_state.received_packet_count, 2),
    ?assertEqual(QueueStats4#nsime_droptail_queue_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats4#nsime_droptail_queue_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats4#nsime_droptail_queue_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats4#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats4#nsime_droptail_queue_state.max_byte_count, MaxBytes),

    {Packet2, QueueState5} = nsime_droptail_queue:dequeue_packet(QueueState4),
    ?assert(nsime_droptail_queue:is_empty(QueueState5)),
    QueueStats5 = nsime_droptail_queue:get_statistics(QueueState5),
    ?assertEqual(QueueStats5#nsime_droptail_queue_state.current_packet_count, 0),
    ?assertEqual(QueueStats5#nsime_droptail_queue_state.current_byte_count, 0),
    ?assertEqual(QueueStats5#nsime_droptail_queue_state.received_packet_count, 2),
    ?assertEqual(QueueStats5#nsime_droptail_queue_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats5#nsime_droptail_queue_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats5#nsime_droptail_queue_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats5#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats5#nsime_droptail_queue_state.max_byte_count, MaxBytes),

    ?assertEqual(nsime_droptail_queue:dequeue_packet(QueueState5), {none, QueueState5}),
    ?assert(nsime_droptail_queue:is_empty(QueueState5)),
    QueueStats6 = nsime_droptail_queue:get_statistics(QueueState5),
    ?assertEqual(QueueStats6#nsime_droptail_queue_state.current_packet_count, 0),
    ?assertEqual(QueueStats6#nsime_droptail_queue_state.current_byte_count, 0),
    ?assertEqual(QueueStats6#nsime_droptail_queue_state.received_packet_count, 2),
    ?assertEqual(QueueStats6#nsime_droptail_queue_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats6#nsime_droptail_queue_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats6#nsime_droptail_queue_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats6#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats6#nsime_droptail_queue_state.max_byte_count, MaxBytes).

test_drop_packet(_) ->
    MaxPackets = 3,
    MaxBytes = 3000,
    QueueState = #nsime_droptail_queue_state{max_packet_count = MaxPackets, max_byte_count = MaxBytes},
    ?assert(is_record(QueueState, nsime_droptail_queue_state)),
    Size = 1000,
    Packet1 = create_packet(Size),
    PacketId1 = Packet1#nsime_packet.id,
    Packet2 = create_packet(Size),
    QueueState1 = nsime_droptail_queue:enqueue_packet(QueueState, Packet1),
    ?assert(is_record(QueueState1, nsime_droptail_queue_state)),
    QueueState2 = nsime_droptail_queue:enqueue_packet(QueueState1, Packet2),
    ?assert(is_record(QueueState2, nsime_droptail_queue_state)),

    QueueState3 = nsime_droptail_queue:drop_packet(QueueState2, PacketId1),
    ?assert(is_record(QueueState3, nsime_droptail_queue_state)),
    QueueStats = nsime_droptail_queue:get_statistics(QueueState3),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.current_packet_count, 1),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.current_byte_count, Size),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.received_packet_count, 2),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.dropped_packet_count, 1),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.dropped_byte_count, Size),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.max_byte_count, MaxBytes),

    QueueState3 = nsime_droptail_queue:drop_packet(QueueState3, PacketId1),

    QueueState4 = nsime_droptail_queue:enqueue_packet(QueueState3, Packet1),
    ?assert(is_record(QueueState4, nsime_droptail_queue_state)),
    QueueState5 = nsime_droptail_queue:enqueue_packet(QueueState4, Packet1),
    ?assert(is_record(QueueState5, nsime_droptail_queue_state)),
    QueueState5 = nsime_droptail_queue:drop_packet(QueueState5, PacketId1),
    {Packet2, QueueState6} = nsime_droptail_queue:dequeue_packet(QueueState5),
    ?assert(is_record(QueueState6, nsime_droptail_queue_state)),
    {Packet1, QueueState7} = nsime_droptail_queue:dequeue_packet(QueueState6),
    ?assert(is_record(QueueState7, nsime_droptail_queue_state)),
    {Packet1, QueueState8} = nsime_droptail_queue:dequeue_packet(QueueState7),
    ?assert(is_record(QueueState8, nsime_droptail_queue_state)).

test_dequeue_all_packets(_) ->
    MaxPackets = 3,
    MaxBytes = 3000,
    QueueState = #nsime_droptail_queue_state{max_packet_count = MaxPackets, max_byte_count = MaxBytes},
    ?assert(is_record(QueueState, nsime_droptail_queue_state)),
    Size = 1000,
    Packet1 = create_packet(Size),
    Packet2 = create_packet(Size),
    QueueState1 = nsime_droptail_queue:enqueue_packet(QueueState, Packet1),
    ?assert(is_record(QueueState1, nsime_droptail_queue_state)),
    QueueState2 = nsime_droptail_queue:enqueue_packet(QueueState1, Packet2),
    ?assert(is_record(QueueState2, nsime_droptail_queue_state)),

    QueueStats = nsime_droptail_queue:get_statistics(QueueState2),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.current_packet_count, 2),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.current_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.received_packet_count, 2),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.max_byte_count, MaxBytes),

    QueueState3 = nsime_droptail_queue:dequeue_all_packets(QueueState2),
    QueueStats1 = nsime_droptail_queue:get_statistics(QueueState3),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.current_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.current_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.received_packet_count, 2),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.max_byte_count, MaxBytes).

test_reset_statistics(_) ->
    MaxPackets = 3,
    MaxBytes = 3000,
    QueueState = #nsime_droptail_queue_state{max_packet_count = MaxPackets, max_byte_count = MaxBytes},
    ?assert(is_record(QueueState, nsime_droptail_queue_state)),

    Size = 1000,
    Packet1 = create_packet(Size),
    Packet2 = create_packet(Size),
    QueueState1 = nsime_droptail_queue:enqueue_packet(QueueState, Packet1),
    ?assert(is_record(QueueState1, nsime_droptail_queue_state)),
    QueueState2 = nsime_droptail_queue:enqueue_packet(QueueState1, Packet2),
    ?assert(is_record(QueueState2, nsime_droptail_queue_state)),

    QueueStats = nsime_droptail_queue:get_statistics(QueueState2),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.current_packet_count, 2),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.current_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.received_packet_count, 2),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.received_byte_count, 2*Size),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats#nsime_droptail_queue_state.max_byte_count, MaxBytes),

    QueueState3 = nsime_droptail_queue:reset_statistics(QueueState2),
    ?assertNot(nsime_droptail_queue:is_empty(QueueState3)),
    QueueStats1 = nsime_droptail_queue:get_statistics(QueueState3),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.current_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.current_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.received_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.received_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.dropped_packet_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.dropped_byte_count, 0),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.max_packet_count, MaxPackets),
    ?assertEqual(QueueStats1#nsime_droptail_queue_state.max_byte_count, MaxBytes).

create_packet(Size) ->
    #nsime_packet{
        id = make_ref(),
        size = Size
    }.
