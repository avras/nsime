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

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, create/1, destroy/1, is_empty/1,
         get_device_id/1, set_device_id/2,
         enqueue_packet/2, dequeue_packet/1,
         drop_packet/2, dequeue_all_packets/1,
         get_statistics/1, reset_statistics/1]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

create(QueueState = #nsime_droptail_queue_state{}) ->
    {ok, Pid} = gen_server:start(?MODULE, QueueState, []),
    Pid.

destroy(QueuePid) ->
    gen_server:call(QueuePid, terminate).

is_empty(QueuePid) ->
    gen_server:call(QueuePid, is_empty).

get_device_id(QueuePid) ->
    gen_server:call(QueuePid, get_device_id).

set_device_id(QueuePid, DeviceId) ->
    gen_server:call(QueuePid, {set_device_id, DeviceId}).

enqueue_packet(QueuePid, Packet) ->
    gen_server:call(QueuePid, {enqueue_packet, Packet}).

dequeue_packet(QueuePid) ->
    gen_server:call(QueuePid, dequeue_packet).

drop_packet(QueuePid, PacketId) ->
    gen_server:call(QueuePid, {drop_packet, PacketId}).

dequeue_all_packets(QueuePid) ->
    case dequeue_packet(QueuePid) of
        none ->
            ok;
        #nsime_packet{} ->
            dequeue_all_packets(QueuePid)
    end.

get_statistics(QueuePid) ->
    gen_server:call(QueuePid, get_statistics).

reset_statistics(QueuePid) ->
    gen_server:call(QueuePid, reset_statistics).

init([]) ->
    QueueState = #nsime_droptail_queue_state{},
    {ok, QueueState};

init(QueueState = #nsime_droptail_queue_state{}) ->
    {ok, QueueState}.

handle_call(is_empty, _From, QueueState) ->
    IsEmpty = queue:is_empty(QueueState#nsime_droptail_queue_state.packets),
    {reply, IsEmpty, QueueState};

handle_call(get_device_id, _From, QueueState) ->
    DeviceId = QueueState#nsime_droptail_queue_state.device_id,
    {reply, DeviceId, QueueState};

handle_call({set_device_id, DeviceId}, _From, QueueState) ->
    NewQueueState = QueueState#nsime_droptail_queue_state{device_id = DeviceId},
    {reply, ok, NewQueueState};

handle_call({enqueue_packet, Packet}, _From, QueueState) ->
    CurrentPacketCount = QueueState#nsime_droptail_queue_state.current_packet_count,
    MaxPacketCount = QueueState#nsime_droptail_queue_state.max_packet_count,
    case CurrentPacketCount >= MaxPacketCount of
        false ->
            NewPacketQueue = queue:in(Packet, QueueState#nsime_droptail_queue_state.packets),
            CurrentByteCount = QueueState#nsime_droptail_queue_state.current_byte_count,
            ReceivedByteCount = QueueState#nsime_droptail_queue_state.received_byte_count,
            ReceivedPacketCount = QueueState#nsime_droptail_queue_state.received_packet_count,
            NewQueueState = QueueState#nsime_droptail_queue_state{
                                packets = NewPacketQueue,
                                current_byte_count = CurrentByteCount + Packet#nsime_packet.size,
                                current_packet_count = CurrentPacketCount + 1,
                                received_packet_count = ReceivedPacketCount + 1,
                                received_byte_count = ReceivedByteCount + Packet#nsime_packet.size
            },
            {reply, ok, NewQueueState};
        true ->
            DroppedByteCount = QueueState#nsime_droptail_queue_state.dropped_byte_count,
            DroppedPacketCount = QueueState#nsime_droptail_queue_state.dropped_packet_count,
            NewQueueState = QueueState#nsime_droptail_queue_state{
                                dropped_byte_count = DroppedByteCount + Packet#nsime_packet.size,
                                dropped_packet_count = DroppedPacketCount + 1
            },
            {reply, dropped, NewQueueState}
    end;

handle_call(dequeue_packet, _From, QueueState) ->
    case queue:out(QueueState#nsime_droptail_queue_state.packets) of
        {{value, Packet}, NewPacketQueue} ->
            CurrentPacketCount = QueueState#nsime_droptail_queue_state.current_packet_count,
            CurrentByteCount = QueueState#nsime_droptail_queue_state.current_byte_count,
            NewQueueState = QueueState#nsime_droptail_queue_state{
                                packets = NewPacketQueue,
                                current_byte_count = CurrentByteCount - Packet#nsime_packet.size,
                                current_packet_count = CurrentPacketCount - 1
            },
            {reply, Packet, NewQueueState};
        {empty, _} ->
            {reply, none, QueueState}
    end;

handle_call({drop_packet, PacketId}, _From, QueueState) ->
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
            {reply, none, QueueState};
        1 ->
            {{value, DroppedPacket}, _} = queue:out(MatchingPackets),
            InverseFilterFun = fun(Packet) -> not(FilterFun(Packet)) end,
            NewPacketQueue = queue:filter(InverseFilterFun, QueueState#nsime_droptail_queue_state.packets),
            CurrentPacketCount = QueueState#nsime_droptail_queue_state.current_packet_count,
            CurrentByteCount = QueueState#nsime_droptail_queue_state.current_byte_count,
            DroppedByteCount = QueueState#nsime_droptail_queue_state.dropped_byte_count,
            DroppedPacketCount = QueueState#nsime_droptail_queue_state.dropped_packet_count,
            NewQueueState = QueueState#nsime_droptail_queue_state{
                                packets = NewPacketQueue,
                                current_byte_count = CurrentByteCount - DroppedPacket#nsime_packet.size,
                                current_packet_count = CurrentPacketCount - 1,
                                dropped_byte_count = DroppedByteCount + DroppedPacket#nsime_packet.size,
                                dropped_packet_count = DroppedPacketCount + 1
            },
            {reply, ok, NewQueueState};
        _ ->
            {reply, invalid_state, QueueState}
    end;

handle_call(get_statistics, _From, QueueState) ->
    QueueStateWithoutPackets = QueueState#nsime_droptail_queue_state{packets = queue:new()},
    {reply, QueueStateWithoutPackets, QueueState};
    
handle_call(reset_statistics, _From, QueueState) ->
    NewQueueState = QueueState#nsime_droptail_queue_state{
                        current_packet_count = 0,
                        current_byte_count = 0,
                        received_packet_count = 0,
                        received_byte_count = 0,
                        dropped_packet_count = 0,
                        dropped_byte_count = 0
    },
    {reply, ok, NewQueueState};

handle_call(terminate, _From, QueueState) ->
    {stop, normal, stopped, QueueState}.

handle_cast(_Request, QueueState) ->
    {noreply, QueueState}.

handle_info(_Request, QueueState) ->
    {noreply, QueueState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, QueueState, _Extra) ->
    {ok, QueueState}.
