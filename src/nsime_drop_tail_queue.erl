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
-export([get_statistic/2, reset_statistics/1]).
-export([loop/1]).

create() ->
    QueueState = #nsime_dtq_state{},
    spawn(?MODULE, loop, [QueueState]).

destroy(QueuePid) ->
    % TODO: Remove queue from netdevice
    Ref = erlang:monitor(process, QueuePid),
    exit(QueuePid, kill),
    receive
        {'DOWN', Ref, process, {QueuePid, _Node}, Reason} ->
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
            Packet
    end.

drop_packet(QueuePid, PacketId) ->
    Ref = make_ref(),
    QueuePid ! {drop_packet, self(), PacketId, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

dequeue_all_packets(QueuePid) ->
    case is_empty(QueuePid) of
        false ->
            dequeue_packet(QueuePid),
            dequeue_all_packets(QueuePid);
        true ->
            ok
    end.

get_statistic(QueuePid, StatisticName) ->
    Ref = make_ref(),
    QueuePid ! {get_statistic, self(), StatisticName, Ref},
    receive
        {ok, StatisticValue, Ref} ->
            StatisticValue;
        {none, Ref} ->
            none
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
                    PacketQueue = QueueState#nsime_dtq_state.packets,
                    NewPacketQueue = queue:in(Packet, PacketQueue),
                    From ! {ok, Ref},
                    loop(QueueState#nsime_dtq_state{packets=NewPacketQueue});
                true ->
                    From ! {dropped, Ref},
                    loop(QueueState)
                end
    end.
