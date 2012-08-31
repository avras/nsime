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

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, create/1, destroy/1,
         add_ppp_header/2, process_ppp_header/1,
         ppp_to_ether/1, ether_to_ppp/1,
         set_node/2, get_node/1,
         set_address/2, get_address/1,
         set_data_rate/2, set_interframe_gap/2,
         get_channel/1, set_receive_error_model/2,
         set_queue/2, get_queue/1,
         set_queue_module/2, get_queue_module/1,
         set_mtu/2, get_mtu/1,
         set_device_index/2, get_device_index/1,
         is_link_up/1, attach_channel/2,
         transmit_start/2, transmit_complete/1,
         send_packet/3, receive_packet/2]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

create(DeviceState = #nsime_ptp_netdevice_state{}) ->
    {ok, Pid} = gen_server:start(?MODULE, DeviceState, []),
    Pid.

destroy(DevicePid) ->
    gen_server:call(DevicePid, terminate).

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
    gen_server:call(DevicePid, {set_node, NodePid}).

get_node(DevicePid) ->
    gen_server:call(DevicePid, get_node).

set_address(DevicePid, Address) ->
    gen_server:call(DevicePid, {set_address, Address}).

get_address(DevicePid) ->
    gen_server:call(DevicePid, get_address).

set_data_rate(DevicePid, DataRate) ->
    gen_server:call(DevicePid, {set_data_rate, DataRate}).

set_interframe_gap(DevicePid, InterframeGap) ->
    gen_server:call(DevicePid, {set_interframe_gap, InterframeGap}).

get_channel(DevicePid) ->
    gen_server:call(DevicePid, get_channel).

set_queue(DevicePid, QueuePid) ->
    gen_server:call(DevicePid, {set_queue, QueuePid}).

get_queue(DevicePid) ->
    gen_server:call(DevicePid, get_queue).

set_queue_module(DevicePid, QueueModule) ->
    gen_server:call(DevicePid, {set_queue_module, QueueModule}).

get_queue_module(DevicePid) ->
    gen_server:call(DevicePid, get_queue_module).

set_receive_error_model(DevicePid, ErrorModel) ->
    gen_server:call(DevicePid, {set_receive_error_model, ErrorModel}).

is_link_up(DevicePid) ->
    gen_server:call(DevicePid, is_link_up).

set_mtu(DevicePid, MTU) ->
    gen_server:call(DevicePid, {set_mtu, MTU}).

get_mtu(DevicePid) ->
    gen_server:call(DevicePid, get_mtu).

set_device_index(DevicePid, DeviceIndex) ->
    gen_server:call(DevicePid, {set_device_index, DeviceIndex}).

get_device_index(DevicePid) ->
    gen_server:call(DevicePid, get_device_index).

attach_channel(DevicePid, ChannelPid) ->
    gen_server:call(DevicePid, {attach_channel, ChannelPid}).

transmit_start(DevicePid, Packet = #nsime_packet{}) ->
    gen_server:call(DevicePid, {transmit_start, Packet}).

transmit_complete(DevicePid) ->
    gen_server:call(DevicePid, transmit_complete).

send_packet(DevicePid, Packet = #nsime_packet{}, ProtocolNumber) ->
    PacketWithHeader = add_ppp_header(Packet, ProtocolNumber),
    transmit_start(DevicePid, PacketWithHeader).

receive_packet(_DevicePid, _Packet = #nsime_packet{}) ->
    ok.

init([]) ->
    Queue = nsime_drop_tail_queue:create(),
    DeviceState = #nsime_ptp_netdevice_state{
        queue_module = nsime_drop_tail_queue,
        queue = Queue
    },
    {ok, DeviceState};

init(DeviceState = #nsime_ptp_netdevice_state{}) ->
    {ok, DeviceState}.

handle_call({set_node, NodePid}, _From, DeviceState) ->
    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{node = NodePid},
    {reply, ok, NewDeviceState};

handle_call(get_node, _From, DeviceState) ->
    NodePid = DeviceState#nsime_ptp_netdevice_state.node,
    {reply, NodePid, DeviceState};

handle_call({set_address, Address}, _From, DeviceState) ->
    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{address = Address},
    {reply, ok, NewDeviceState};

handle_call(get_address, _From, DeviceState) ->
    Address = DeviceState#nsime_ptp_netdevice_state.address,
    {reply, Address, DeviceState};

handle_call({set_queue_module, QueueModule}, _From, DeviceState) ->
    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{queue_module = QueueModule},
    {reply, ok, NewDeviceState};

handle_call(get_queue_module, _From, DeviceState) ->
    QueueModule = DeviceState#nsime_ptp_netdevice_state.queue_module,
    {reply, QueueModule, DeviceState};

handle_call({set_data_rate, DataRate}, _From, DeviceState) ->
    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{data_rate = DataRate},
    {reply, ok, NewDeviceState};

handle_call({set_interframe_gap, InterframeGap}, _From, DeviceState) ->
    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{interframe_gap = InterframeGap},
    {reply, ok, NewDeviceState};

handle_call(get_channel, _From, DeviceState) ->
    ChannelPid = DeviceState#nsime_ptp_netdevice_state.channel,
    {reply, ChannelPid, DeviceState};

handle_call({set_queue, QueuePid}, _From, DeviceState) ->
    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{queue = QueuePid},
    {reply, ok, NewDeviceState};

handle_call(get_queue, _From, DeviceState) ->
    QueuePid = DeviceState#nsime_ptp_netdevice_state.queue,
    {reply, QueuePid, DeviceState};

handle_call({set_receive_error_model, ErrorModel}, _From, DeviceState) ->
    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{rx_error_model = ErrorModel},
    {reply, ok, NewDeviceState};

handle_call(is_link_up, _From, DeviceState) ->
    IsLinkUp = DeviceState#nsime_ptp_netdevice_state.link_up,
    {reply, IsLinkUp, DeviceState};

handle_call({set_mtu, MTU}, _From, DeviceState) ->
    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{mtu = MTU},
    {reply, ok, NewDeviceState};

handle_call(get_mtu, _From, DeviceState) ->
    MTU = DeviceState#nsime_ptp_netdevice_state.mtu,
    {reply, MTU, DeviceState};

handle_call({set_device_index, DeviceIndex}, _From, DeviceState) ->
    NewDeviceState = DeviceState#nsime_ptp_netdevice_state{device_index = DeviceIndex},
    {reply, ok, NewDeviceState};

handle_call(get_device_index, _From, DeviceState) ->
    DeviceIndex = DeviceState#nsime_ptp_netdevice_state.device_index,
    {reply, DeviceIndex, DeviceState};

handle_call({attach_channel, ChannelPid}, _From, DeviceState) ->
    case nsime_ptp_channel:attach_netdevice(ChannelPid, self()) of
        ok ->
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{channel = ChannelPid, link_up = true},
           {reply, ok, NewDeviceState}; 
        none ->
           {reply, none, DeviceState}
    end;

handle_call({transmit_start, Packet}, _From, DeviceState) ->
    case {DeviceState#nsime_ptp_netdevice_state.tx_state, DeviceState#nsime_ptp_netdevice_state.link_up} of
        {ready, true} ->
            Length = Packet#nsime_packet.size,
            DataRate = DeviceState#nsime_ptp_netdevice_state.data_rate,
            TxTime = nsime_data_rate:calc_tx_time(DataRate, Length),
            TxCompleteEvent = #nsime_event{
                time = TxTime,
                module = nsime_ptp_netdevice,
                function = transmit_complete,
                arguments = [self()],
                eventid = make_ref()
            },
            nsime_simulator:schedule(TxTime, TxCompleteEvent),
            Channel = DeviceState#nsime_ptp_netdevice_state.channel,
            nsime_ptp_channel:transmit(Channel, Packet, self(), TxTime),
            NewDeviceState = DeviceState#nsime_ptp_netdevice_state{
                tx_state = busy,
                current_packet = Packet
            },
            {reply, true, NewDeviceState};
        {busy, true} ->
            QueueModule = DeviceState#nsime_ptp_netdevice_state.queue_module,
            QueuePid = DeviceState#nsime_ptp_netdevice_state.queue,
            QueueModule:enqueue_packet(QueuePid, Packet),
            {reply, true, DeviceState};
        {_, false} ->
            {reply, false, DeviceState}
    end;

handle_call(transmit_complete, _From, DeviceState) ->
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
            {reply, true, NewDeviceState};
        ready ->
            {reply, false, DeviceState}
    end;

handle_call(terminate, _From, DeviceState) ->
    {stop, normal, stopped, DeviceState}.

handle_cast(_Request, DeviceState) ->
    {noreply, DeviceState}.

handle_info(_Request, DeviceState) ->
    {noreply, DeviceState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, DeviceState, _Extra) ->
    {ok, DeviceState}.
