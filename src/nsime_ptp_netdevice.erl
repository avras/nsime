%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Point-to-point netdevice module
%% Author : Saravanan Vijayakumaran

-module(nsime_ptp_netdevice).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_packet.hrl").
-include("nsime_ptp_netdevice_state.hrl").

-export([create/0, create/1, destroy/1]).
-export([add_ppp_header/2, process_ppp_header/1, ppp_to_ether/1, ether_to_ppp/1]).
-export([set_node/2, get_node/1]).
-export([set_address/2, get_address/1]).
-export([set_data_rate/2, set_interframe_gap/2]).
-export([get_channel/1, set_receive_error_model/2]).
-export([set_queue/2, get_queue/1, set_queue_module/2, get_queue_module/1]).
-export([set_mtu/2, get_mtu/1]).
-export([set_device_index/2, get_device_index/1]).
-export([is_link_up/1, attach_channel/2]).
-export([transmit_start/2, transmit_complete/1]).
-export([send_packet/3, receive_packet/2]).
-export([loop/1]).


create() ->
    DeviceState = #nsime_ptp_netdevice_state{},
    spawn(?MODULE, loop, [DeviceState]).

create(DeviceState = #nsime_ptp_netdevice_state{}) ->
    spawn(?MODULE, loop, [DeviceState]).

destroy(DevicePid) ->
    Ref = erlang:monitor(process, DevicePid),
    exit(DevicePid, kill),
    receive
        {'DOWN', Ref, process, DevicePid, Reason} ->
            Reason
    end.

add_ppp_header(Packet = #nsime_packet{data = Data, size = Size}, EthernetProtocolNumber) ->
    PPPProtocolNumber = ether_to_ppp(EthernetProtocolNumber),
    Packet#nsime_packet{data = <<PPPProtocolNumber:16, Data/binary>>, size = Size + 2}.

process_ppp_header(Packet = #nsime_packet{data = Data, size = Size}) ->
    <<PPPHeader:16, Payload/binary>> = Data,
    EthernetProtocolNumber = ppp_to_ether(PPPHeader),
    NewPacket = Packet#nsime_packet{data = Payload, size = Size - 2},
    {EthernetProtocolNumber, NewPacket}.

ppp_to_ether(ProtocolNumber) ->
    case <<ProtocolNumber:16>> of
        <<16#0021:16>> -> 16#0800;
        <<16#0057:16>> -> 16#86DD;
        _ -> erlang:error(undefined_protocol)
    end.

ether_to_ppp(ProtocolNumber) ->
    case <<ProtocolNumber:16>> of
        <<16#0800:16>> -> 16#0021;
        <<16#86DD:16>> -> 16#0057;
        _ -> erlang:error(undefined_protocol)
    end.

set_node(DevicePid, NodePid) ->
    Ref = make_ref(),
    DevicePid ! {set_node, self(), NodePid, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_node(DevicePid) ->
    Ref = make_ref(),
    DevicePid ! {get_node, self(), Ref},
    receive
        {ok, NodePid, Ref} ->
            NodePid
    end.

set_address(DevicePid, Address) ->
    Ref = make_ref(),
    DevicePid ! {set_address, self(), Address, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_address(DevicePid) ->
    Ref = make_ref(),
    DevicePid ! {get_address, self(), Ref},
    receive
        {ok, Address, Ref} ->
            Address
    end.

set_data_rate(DevicePid, DataRate) ->
    Ref = make_ref(),
    DevicePid ! {set_data_rate, self(), DataRate, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

set_interframe_gap(DevicePid, InterframeGap) ->
    Ref = make_ref(),
    DevicePid ! {set_interframe_gap, self(), InterframeGap, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_channel(DevicePid) ->
    Ref = make_ref(),
    DevicePid ! {get_channel, self(), Ref},
    receive
        {ok, ChannelPid, Ref} ->
            ChannelPid
    end.

set_queue(DevicePid, QueuePid) ->
    Ref = make_ref(),
    DevicePid ! {set_queue, self(), QueuePid, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_queue(DevicePid) ->
    Ref = make_ref(),
    DevicePid ! {get_queue, self(), Ref},
    receive
        {ok, QueuePid, Ref} ->
            QueuePid
    end.

set_queue_module(DevicePid, QueueModule) ->
    Ref = make_ref(),
    DevicePid ! {set_queue_module, self(), QueueModule, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_queue_module(DevicePid) ->
    Ref = make_ref(),
    DevicePid ! {get_queue_module, self(), Ref},
    receive
        {ok, QueueModule, Ref} ->
            QueueModule
    end.

set_receive_error_model(DevicePid, ErrorModel) ->
    Ref = make_ref(),
    DevicePid ! {set_receive_error_model, self(), ErrorModel, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

is_link_up(DevicePid) ->
    Ref = make_ref(),
    DevicePid ! {is_link_up, self(), Ref},
    receive
        {ok, IsLinkUp, Ref} ->
            IsLinkUp
    end.

set_mtu(DevicePid, MTU) ->
    Ref = make_ref(),
    DevicePid ! {set_mtu, self(), MTU, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_mtu(DevicePid) ->
    Ref = make_ref(),
    DevicePid ! {get_mtu, self(), Ref},
    receive
        {ok, MTU, Ref} ->
            MTU
    end.

set_device_index(DevicePid, DeviceIndex) ->
    Ref = make_ref(),
    DevicePid ! {set_device_index, self(), DeviceIndex, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_device_index(DevicePid) ->
    Ref = make_ref(),
    DevicePid ! {get_device_index, self(), Ref},
    receive
        {ok, DeviceIndex, Ref} ->
            DeviceIndex
    end.

attach_channel(DevicePid, ChannelPid) ->
    Ref = make_ref(),
    DevicePid ! {attach_channel, self(), ChannelPid, Ref},
    receive
        {none, Ref} ->
            none;
        {ok, Ref} ->
            ok
    end.

transmit_start(DevicePid, Packet = #nsime_packet{}) ->
    Ref = make_ref(),
    DevicePid ! {transmit_start, self(), Packet, Ref},
    receive
        {Result, Ref} ->
            Result
    end.

transmit_complete(DevicePid) ->
    Ref = make_ref(),
    DevicePid ! {transmit_complete, self(), Ref},
    receive
        {Result, Ref} ->
            Result
    end.

send_packet(DevicePid, Packet = #nsime_packet{}, <<ProtocolNumber:16>>) ->
    PacketWithHeader = add_ppp_header(Packet, ProtocolNumber),
    transmit_start(DevicePid, PacketWithHeader).

receive_packet(_DevicePid, _Packet = #nsime_packet{}) ->
    ok.

loop(DeviceState = #nsime_ptp_netdevice_state{}) ->
    receive
        {set_node, From, NodePid, Ref} ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{node = NodePid},
            From ! {ok, Ref},
            loop(NewDeviceState);
        {get_node, From, Ref} ->
            NodePid = DeviceState#nsime_ptp_netdevice_state.node,
            From ! {ok, NodePid, Ref},
            loop(DeviceState);
        {set_address, From, Address, Ref} ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{address = Address},
            From ! {ok, Ref},
            loop(NewDeviceState);
        {get_address, From, Ref} ->
            Address = DeviceState#nsime_ptp_netdevice_state.address,
            From ! {ok, Address, Ref},
            loop(DeviceState);
        {set_queue_module, From, QueueModule, Ref} ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{queue_module = QueueModule},
            From ! {ok, Ref},
            loop(NewDeviceState);
        {get_queue_module, From, Ref} ->
            QueueModule = DeviceState#nsime_ptp_netdevice_state.queue_module,
            From ! {ok, QueueModule, Ref},
            loop(DeviceState);
        {set_data_rate, From, DataRate, Ref} ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{data_rate = DataRate},
            From ! {ok, Ref},
            loop(NewDeviceState);
        {set_interframe_gap, From, InterframeGap, Ref} ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{interframe_gap = InterframeGap},
            From ! {ok, Ref},
            loop(NewDeviceState);
        {get_channel, From, Ref} ->
            ChannelPid = DeviceState#nsime_ptp_netdevice_state.channel,
            From ! {ok, ChannelPid, Ref},
            loop(DeviceState);
        {set_queue, From, QueuePid, Ref} ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{queue = QueuePid},
            From ! {ok, Ref},
            loop(NewDeviceState);
        {get_queue, From, Ref} ->
            QueuePid = DeviceState#nsime_ptp_netdevice_state.queue,
            From ! {ok, QueuePid, Ref},
            loop(DeviceState);
        {set_receive_error_model, From, ErrorModel, Ref} ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{rx_error_model = ErrorModel},
            From ! {ok, Ref},
            loop(NewDeviceState);
        {is_link_up, From, Ref} ->
            IsLinkUp = DeviceState#nsime_ptp_netdevice_state.link_up,
            From ! {ok, IsLinkUp, Ref},
            loop(DeviceState);
        {set_mtu, From, MTU, Ref} ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{mtu = MTU},
            From ! {ok, Ref},
            loop(NewDeviceState);
        {get_mtu, From, Ref} ->
            MTU = DeviceState#nsime_ptp_netdevice_state.mtu,
            From ! {ok, MTU, Ref},
            loop(DeviceState);
        {set_device_index, From, DeviceIndex, Ref} ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{device_index = DeviceIndex},
            From ! {ok, Ref},
            loop(NewDeviceState);
        {get_device_index, From, Ref} ->
            DeviceIndex = DeviceState#nsime_ptp_netdevice_state.device_index,
            From ! {ok, DeviceIndex, Ref},
            loop(DeviceState);
        {attach_channel, From, ChannelPid, Ref} ->
            case nsime_ptp_channel:attach_netdevice(ChannelPid, self()) of
                ok ->
                    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{channel = ChannelPid, link_up = true},
                    From ! {ok, Ref},
                    loop(NewDeviceState);
                none ->
                    From ! {none, Ref},
                    loop(DeviceState)
            end;
        {transmit_start, From, Packet, Ref} ->
            case {DeviceState#nsime_ptp_netdevice_state.tx_state, DeviceState#nsime_ptp_netdevice_state.link_up} of
                {ready, true} ->
                    Length = Packet#nsime_packet.size,
                    DataRate = DeviceState#nsime_ptp_netdevice_state.data_rate,
                    TxTime = nsime_data_rate:calc_tx_time(DataRate, Length),
                    TxCompleteTime = nsime_time:add(
                        TxTime,
                        DeviceState#nsime_ptp_netdevice_state.interframe_gap
                    ),
                    TxCompleteEvent = #nsime_event{
                        time = TxCompleteTime,
                        module = nsime_ptp_netdevice,
                        function = transmit_complete,
                        arguments = [self()],
                        eventid = make_ref()
                    },
                    nsime_simulator:schedule(TxCompleteTime, TxCompleteEvent),
                    Channel = DeviceState#nsime_ptp_netdevice_state.channel,
                    nsime_ptp_channel:transmit(Channel, Packet, self(), TxTime),
                    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{
                        tx_state = busy,
                        current_packet = Packet
                    },
                    From ! {true, Ref},
                    loop(NewDeviceState);
                {busy, true} ->
                    QueueModule = DeviceState#nsime_ptp_netdevice_state.queue_module,
                    QueuePid = DeviceState#nsime_ptp_netdevice_state.queue,
                    QueueModule:enqueue_packet(QueuePid, Packet),
                    From ! {true, Ref},
                    loop(DeviceState);
                {_, false} ->
                    From ! {false, Ref},
                    loop(DeviceState)
            end;
        {transmit_complete, From, Ref} ->
            case DeviceState#nsime_ptp_netdevice_state.tx_state of
                busy ->
                    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{
                        tx_state = ready,
                        current_packet = none
                    },
                    QueueModule = DeviceState#nsime_ptp_netdevice_state.queue_module,
                    QueuePid = DeviceState#nsime_ptp_netdevice_state.queue,
                    Packet = QueueModule:dequeue_packet(QueuePid),
                    case Packet of
                        none ->
                            ok;
                        _ ->
                            InterframeGap = DeviceState#nsime_ptp_netdevice_state.interframe_gap,
                            NextTxEvent = #nsime_event{
                                time = InterframeGap,
                                module = nsime_ptp_netdevice,
                                function = transmit_start,
                                arguments = [self(), Packet],
                                eventid = make_ref()
                            },
                            nsime_simulator:schedule(InterframeGap, NextTxEvent)
                    end,
                    From ! {true, Ref},
                    loop(NewDeviceState);
                ready ->
                    From ! {false, Ref},
                    loop(DeviceState)
            end
    end.
