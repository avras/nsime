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

%% Purpose : Drop tail queue module
%% Author : Saravanan Vijayakumaran

-module(nsime_droptail_queue).
-author("Saravanan Vijayakumaran").

-include("nsime_droptail_queue_state.hrl").
-include("nsime_packet.hrl").

-export([create/0, is_empty/1,
         enqueue_packet/2, dequeue_packet/1,
         drop_packet/2, dequeue_all_packets/1,
         get_statistics/1, reset_statistics/1]).

create() ->
    #nsime_droptail_queue_state{}.

is_empty(QueueState) ->
    queue:is_empty(QueueState#nsime_droptail_queue_state.packets).

enqueue_packet(QueueState, Packet) ->
    CurrentPacketCount = QueueState#nsime_droptail_queue_state.current_packet_count,
    MaxPacketCount = QueueState#nsime_droptail_queue_state.max_packet_count,
    case CurrentPacketCount >= MaxPacketCount of
        false ->
            NewPacketQueue = queue:in(Packet, QueueState#nsime_droptail_queue_state.packets),
            CurrentByteCount = QueueState#nsime_droptail_queue_state.current_byte_count,
            ReceivedByteCount = QueueState#nsime_droptail_queue_state.received_byte_count,
            ReceivedPacketCount = QueueState#nsime_droptail_queue_state.received_packet_count,
            QueueState#nsime_droptail_queue_state{
                packets = NewPacketQueue,
                current_byte_count = CurrentByteCount + Packet#nsime_packet.size,
                current_packet_count = CurrentPacketCount + 1,
                received_packet_count = ReceivedPacketCount + 1,
                received_byte_count = ReceivedByteCount + Packet#nsime_packet.size
            };
        true ->
            DroppedByteCount = QueueState#nsime_droptail_queue_state.dropped_byte_count,
            DroppedPacketCount = QueueState#nsime_droptail_queue_state.dropped_packet_count,
            QueueState#nsime_droptail_queue_state{
                dropped_byte_count = DroppedByteCount + Packet#nsime_packet.size,
                dropped_packet_count = DroppedPacketCount + 1
            }
    end.

dequeue_packet(QueueState) ->
    case queue:out(QueueState#nsime_droptail_queue_state.packets) of
        {{value, Packet}, NewPacketQueue} ->
            CurrentPacketCount = QueueState#nsime_droptail_queue_state.current_packet_count,
            CurrentByteCount = QueueState#nsime_droptail_queue_state.current_byte_count,
            NewQueueState = QueueState#nsime_droptail_queue_state{
                packets = NewPacketQueue,
                current_byte_count = CurrentByteCount - Packet#nsime_packet.size,
                current_packet_count = CurrentPacketCount - 1
            },
            {Packet, NewQueueState};
        {empty, _} ->
            {none, QueueState}
    end.

drop_packet(QueueState, PacketId) ->
    FilterFun =
    fun(Packet) ->
        case Packet#nsime_packet.id of
            PacketId ->
                true;
            _ ->
                false
        end
    end,
    MatchingPackets = queue:filter(FilterFun, QueueState#nsime_droptail_queue_state.packets),
    case queue:len(MatchingPackets) of
        0 ->
            QueueState;
        1 ->
            {{value, DroppedPacket}, _} = queue:out(MatchingPackets),
            InverseFilterFun = fun(Packet) -> not(FilterFun(Packet)) end,
            NewPacketQueue = queue:filter(InverseFilterFun, QueueState#nsime_droptail_queue_state.packets),
            CurrentPacketCount = QueueState#nsime_droptail_queue_state.current_packet_count,
            CurrentByteCount = QueueState#nsime_droptail_queue_state.current_byte_count,
            DroppedByteCount = QueueState#nsime_droptail_queue_state.dropped_byte_count,
            DroppedPacketCount = QueueState#nsime_droptail_queue_state.dropped_packet_count,
            QueueState#nsime_droptail_queue_state{
                packets = NewPacketQueue,
                current_byte_count = CurrentByteCount - DroppedPacket#nsime_packet.size,
                current_packet_count = CurrentPacketCount - 1,
                dropped_byte_count = DroppedByteCount + DroppedPacket#nsime_packet.size,
                dropped_packet_count = DroppedPacketCount + 1
            };
        _ ->
            QueueState
    end.

dequeue_all_packets(QueueState) ->
    case dequeue_packet(QueueState) of
        {none, _NewQueueState} ->
            ok;
        {#nsime_packet{}, NewQueueState} ->
            dequeue_all_packets(NewQueueState)
    end.

get_statistics(QueueState) ->
    QueueState#nsime_droptail_queue_state{packets = queue:new()}.

reset_statistics(QueueState) ->
    QueueState#nsime_droptail_queue_state{
        current_packet_count = 0,
        current_byte_count = 0,
        received_packet_count = 0,
        received_byte_count = 0,
        dropped_packet_count = 0,
        dropped_byte_count = 0
    }.
