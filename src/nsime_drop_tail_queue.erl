%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Drop tail queue module
%% Author : Saravanan Vijayakumaran

-module(nsime_drop_tail_queue).
-author("Saravanan Vijayakumaran").

-include("nsime_dtq_state.hrl").
-include("nsime_packet.hrl").

-export([create/0, destroy/1, is_empty/1]).
-export([enqueue_packet/2, dequeue_packet/1, drop_packet/2, dequeue_all_packets/1]).
-export([get_statistics/1, reset_statistics/1]).
-export([loop/1]).

create() ->
    QueueState = #nsime_dtq_state{},
    spawn(?MODULE, loop, [QueueState]).

destroy(QueuePid) ->
    % TODO: Remove queue from netdevice
    Ref = erlang:monitor(process, QueuePid),
    exit(QueuePid, kill),
    receive
        {'DOWN', Ref, process, QueuePid, Reason} ->
            Reason
    end.

is_empty(QueuePid) ->
    Ref = make_ref(),
    QueuePid ! {is_empty, self(), Ref},
    receive
        {is_empty, IsEmpty, Ref} ->
            IsEmpty
    end.

enqueue_packet(QueuePid, Packet = #nsime_packet{}) ->
    Ref = make_ref(),
    QueuePid ! {enqueue_packet, self(), Packet, Ref},
    receive
        {ok, Ref} ->
            ok;
        {dropped, Ref} ->
            dropped
    end.

dequeue_packet(QueuePid) ->
    Ref = make_ref(),
    QueuePid ! {dequeue_packet, self(), Ref},
    receive
        {ok, Packet, Ref} ->
            Packet;
        {none, Ref} ->
            none
    end.

drop_packet(QueuePid, PacketId) ->
    Ref = make_ref(),
    QueuePid ! {drop_packet, self(), PacketId, Ref},
    receive
        {ok, Ref} ->
            ok;
        {none, Ref} ->
            none
    end.

dequeue_all_packets(QueuePid) ->
    case dequeue_packet(QueuePid) of
        none ->
            ok;
        #nsime_packet{} ->
            dequeue_all_packets(QueuePid)
    end.

get_statistics(QueuePid) ->
    Ref = make_ref(),
    QueuePid ! {get_statistics, self(), Ref},
    receive
        {ok, Statistics, Ref} ->
            Statistics
    end.

reset_statistics(QueuePid) ->
    Ref = make_ref(),
    QueuePid ! {reset_statistics, self(), Ref},
    receive
        {ok, Ref} ->
            ok
    end.

loop(QueueState) ->
    receive
        {is_empty, From, Ref} ->
            IsEmpty = queue:is_empty(QueueState#nsime_dtq_state.packets),
            From ! {is_empty, IsEmpty, Ref};
        {enqueue_packet, From, Packet, Ref} ->
            CurrentPacketCount = QueueState#nsime_dtq_state.current_packet_count,
            MaxPacketCount = QueueState#nsime_dtq_state.max_packet_count,
            case CurrentPacketCount >= MaxPacketCount of
                false ->
                    NewPacketQueue = queue:in(Packet, QueueState#nsime_dtq_state.packets),
                    CurrentByteCount = QueueState#nsime_dtq_state.current_byte_count,
                    ReceivedByteCount = QueueState#nsime_dtq_state.received_byte_count,
                    ReceivedPacketCount = QueueState#nsime_dtq_state.received_packet_count,
                    NewQueueState = QueueState#nsime_dtq_state{
                                        packets = NewPacketQueue,
                                        current_byte_count = CurrentByteCount + Packet#nsime_packet.size,
                                        current_packet_count = CurrentPacketCount + 1,
                                        received_packet_count = ReceivedPacketCount + 1,
                                        received_byte_count = ReceivedByteCount + 1
                    },
                    From ! {ok, Ref},
                    loop(NewQueueState);
                true ->
                    DroppedByteCount = QueueState#nsime_dtq_state.dropped_byte_count,
                    DroppedPacketCount = QueueState#nsime_dtq_state.dropped_packet_count,
                    NewQueueState = QueueState#nsime_dtq_state{
                                        dropped_byte_count = DroppedByteCount + Packet#nsime_packet.size,
                                        dropped_packet_count = DroppedPacketCount + 1
                    },
                    From ! {dropped, Ref},
                    loop(NewQueueState)
            end;
        {dequeue_packet, From, Ref} ->
            case queue:out(QueueState#nsime_dtq_state.packets) of
                {{value, Packet}, NewPacketQueue} ->
                    CurrentPacketCount = QueueState#nsime_dtq_state.current_packet_count,
                    CurrentByteCount = QueueState#nsime_dtq_state.current_byte_count,
                    NewQueueState = QueueState#nsime_dtq_state{
                                        packets = NewPacketQueue,
                                        current_byte_count = CurrentByteCount - Packet#nsime_packet.size,
                                        current_packet_count = CurrentPacketCount - 1
                    },
                    From ! {ok, Packet, Ref},
                    loop(NewQueueState);
                {empty, _} ->
                    From ! {none, Ref},
                    loop(QueueState)
            end;
        {drop_packet, From, PacketId, Ref} ->
            FilterFun =
            fun(Packet) ->
                case Packet#nsime_packet.id of
                    PacketId ->
                        false;
                    _ ->
                        true
                end
            end,
            InverseFilterFun = fun(Packet) -> not(FilterFun(Packet)) end,
            [Packet | []] = queue:filter(InverseFilterFun, QueueState#nsime_dtq_state.packets),
            case Packet of
                [] ->
                    From ! {none, Ref},
                    loop(QueueState);
                _ ->
                    NewPacketQueue = queue:filter(FilterFun, QueueState#nsime_dtq_state.packets),
                    CurrentPacketCount = QueueState#nsime_dtq_state.current_packet_count,
                    CurrentByteCount = QueueState#nsime_dtq_state.current_byte_count,
                    DroppedByteCount = QueueState#nsime_dtq_state.dropped_byte_count,
                    DroppedPacketCount = QueueState#nsime_dtq_state.dropped_packet_count,
                    NewQueueState = QueueState#nsime_dtq_state{
                                        packets = NewPacketQueue,
                                        current_byte_count = CurrentByteCount - Packet#nsime_packet.size,
                                        current_packet_count = CurrentPacketCount - 1,
                                        dropped_byte_count = DroppedByteCount + Packet#nsime_packet.size,
                                        dropped_packet_count = DroppedPacketCount + 1
                    },
                    From ! {ok, Ref},
                    loop(NewQueueState)
            end;
        {get_statistics, From, Ref} ->
            QueueStateWithoutPackets = QueueState#nsime_dtq_state{packets = queue:empty()},
            From ! {ok, QueueStateWithoutPackets, Ref},
            loop(QueueState);
        {reset_statistics, From, Ref} ->
            NewQueueState = QueueState#nsime_dtq_state{
                                current_packet_count = 0,
                                current_byte_count = 0,
                                received_packet_count = 0,
                                received_byte_count = 0,
                                dropped_packet_count = 0,
                                dropped_byte_count = 0
            },
            From ! {ok, Ref},
            loop(NewQueueState)
    end.
