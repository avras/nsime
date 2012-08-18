%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Point-to-point channel module
%% Author : Saravanan Vijayakumaran

-module(nsime_ptp_channel).
-author("Saravanan Vijayakumaran").

-include("nsime_event.hrl").
-include("nsime_packet.hrl").
-include("nsime_ptp_channel_state.hrl").

-export([create/0, create/1, destroy/1]).
-export([get_netdevice/2, get_netdevice_count/1]).
-export([get_channel_delay/1, set_channel_delay/2]).
-export([attach_netdevice/2, transmit/4]).
-export([loop/1]).


create() ->
    ChannelState = #nsime_ptp_channel_state{},
    spawn(?MODULE, loop, [ChannelState]).

create(ChannelState = #nsime_ptp_channel_state{}) ->
    spawn(?MODULE, loop, [ChannelState]).

destroy(ChannelPid) ->
    Ref = erlang:monitor(process, ChannelPid),
    exit(ChannelPid, kill),
    receive
        {'DOWN', Ref, process, {ChannelPid, _Node}, Reason} ->
            Reason
    end.

get_netdevice(ChannelPid, DeviceIndex) when 
    is_integer(DeviceIndex), 
    DeviceIndex >= 0, 
    DeviceIndex < 2 
    ->
    Ref = make_ref(),
    ChannelPid ! {get_netdevice, self(), DeviceIndex, Ref},
    receive
        {netdevice, DevicePid, Ref} ->
            DevicePid;
        {none, Ref} ->
            none
    end;
get_netdevice(_ChannelPid, _DeviceIndex) ->
    erlang:error(invalid_argument).

get_channel_delay(ChannelPid) ->
    Ref = make_ref(),
    ChannelPid ! {get_channel_delay, self(), Ref},
    receive
        {channel_delay, Delay, Ref} ->
            Delay
    end.

set_channel_delay(ChannelPid, Delay) ->
    Ref = make_ref(),
    ChannelPid ! {set_channel_delay, self(), Delay, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_netdevice_count(ChannelPid) ->
    Ref = make_ref(),
    ChannelPid ! {get_netdevice_count, self(), Ref},
    receive
        {netdevice_count, DeviceCount, Ref} ->
            DeviceCount
    end.

attach_netdevice(ChannelPid, DevicePid) ->
    Ref = make_ref(),
    case get_netdevice_count(ChannelPid) of
      2 ->
          none;
      _Count ->  
          ChannelPid ! {attach_netdevice, self(), DevicePid, Ref},
          receive
              {ok, Ref} ->
                  ok
          end
    end.
    

transmit(
    ChannelPid, 
    Packet = #nsime_packet{}, 
    SourceDevicePid, 
    TxTime
    ) ->
    Ref = make_ref(),
    ChannelPid ! {transmit, self(), Packet, SourceDevicePid, TxTime, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

loop(ChannelState = #nsime_ptp_channel_state{}) ->
    receive
        {get_netdevice, From, DeviceIndex, Ref} ->
            Devices = ChannelState#nsime_ptp_channel_state.devices,
            case DeviceIndex of
                0 ->
                    {DevicePid, _OtherDevicePid} = Devices;
                1 ->
                    {_OtherDevicePid, DevicePid} = Devices
            end,
            case DevicePid of
                none ->
                    From ! {none, Ref};
                _Pid ->
                    From ! {netdevice, DevicePid, Ref}
            end,
            loop(ChannelState);
        {get_channel_delay, From, Ref} ->
            From ! {channel_delay, ChannelState#nsime_ptp_channel_state.delay, Ref},
            loop(ChannelState);
        {set_channel_delay, From, Delay, Ref} ->
            NewChannelState = ChannelState#nsime_ptp_channel_state{delay = Delay},
            From ! {ok, Ref},
            loop(NewChannelState);
        {get_netdevice_count, From, Ref} ->
            Devices = ChannelState#nsime_ptp_channel_state.devices,
            DeviceCount = case Devices of
                {none, none} -> 0;
                {_DevicePid, none} -> 1;
                {_DevicePid1, _DevicePid2} -> 2
            end,
            From ! {netdevice_count, DeviceCount, Ref},
            loop(ChannelState);
        {attach_netdevice, From, DevicePid, Ref} ->
            Devices = ChannelState#nsime_ptp_channel_state.devices,
            NewDevices = case Devices of
                {none, none} -> {DevicePid, none};
                {FirstDevicePid, none} -> {FirstDevicePid, DevicePid}
            end,
            NewChannelState = ChannelState#nsime_ptp_channel_state{devices = NewDevices},
            From ! {ok, Ref},
            loop(NewChannelState);
        {transmit, From, Packet, SourceDevicePid, TxTime, Ref} ->
            Devices = ChannelState#nsime_ptp_channel_state.devices,
            DestDevicePid = case Devices of 
                {SourceDevicePid, DevicePid} -> DevicePid;
                {DevicePid, SourceDevicePid} -> DevicePid
            end,
            Delay = #nsime_ptp_channel_state.delay,
            EventTime = nsime_time:add(TxTime, Delay),
            ReceiveEvent = #nsime_event{
                time = EventTime,
                module = nsime_ptp_netdevice,
                function = receive_packet,
                arguments = [DestDevicePid, EventTime, Packet],
                eventid = make_ref()
            },
            nsime_simulator:schedule(TxTime, ReceiveEvent),
            From ! {ok, Ref},
            loop(ChannelState)
    end.
