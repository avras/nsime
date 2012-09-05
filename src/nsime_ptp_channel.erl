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

%% Purpose : Point-to-point channel module
%% Author : Saravanan Vijayakumaran

-module(nsime_ptp_channel).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_packet.hrl").
-include("nsime_ptp_channel_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, create/1, destroy/1,
         get_netdevice/2, get_netdevice_count/1,
         get_channel_delay/1, set_channel_delay/2,
         attach_netdevice/2, transmit/4]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

create(ChannelState = #nsime_ptp_channel_state{}) ->
    {ok, Pid} = gen_server:start(?MODULE, ChannelState, []),
    Pid.

destroy(ChannelPid) ->
    gen_server:call(ChannelPid, terminate).

get_netdevice(ChannelPid, DeviceIndex) when 
    is_integer(DeviceIndex), 
    DeviceIndex >= 0, 
    DeviceIndex < 2 
    ->
    gen_server:call(ChannelPid, {get_netdevice, DeviceIndex});
get_netdevice(_ChannelPid, _DeviceIndex) ->
    erlang:error(invalid_argument).

get_channel_delay(ChannelPid) ->
    gen_server:call(ChannelPid, get_channel_delay).

set_channel_delay(ChannelPid, Delay) ->
    gen_server:call(ChannelPid, {set_channel_delay, Delay}).

get_netdevice_count(ChannelPid) ->
    gen_server:call(ChannelPid, get_netdevice_count).

attach_netdevice(ChannelPid, DevicePid) ->
    case get_netdevice_count(ChannelPid) of
      2 ->
          none;
      _Count ->  
          gen_server:call(ChannelPid, {attach_netdevice, DevicePid})
    end.

transmit(
    ChannelPid, 
    Packet = #nsime_packet{}, 
    SourceDevicePid, 
    TxTime
    ) ->
    gen_server:call(ChannelPid, {transmit, Packet, SourceDevicePid, TxTime}).

init([]) ->
    ChannelState = #nsime_ptp_channel_state{},
    {ok, ChannelState};

init(ChannelState = #nsime_ptp_channel_state{}) ->
    {ok, ChannelState}.

handle_call({get_netdevice, DeviceIndex}, _From, ChannelState) ->
    Devices = ChannelState#nsime_ptp_channel_state.devices,
    case DeviceIndex of
        0 ->
            {DevicePid, _OtherDevicePid} = Devices;
        1 ->
            {_OtherDevicePid, DevicePid} = Devices
    end,
    case DevicePid of
        none ->
            {reply, none, ChannelState}; 
        _Pid ->
            {reply, DevicePid, ChannelState}
    end;

handle_call(get_channel_delay, _From, ChannelState) ->
        {reply, ChannelState#nsime_ptp_channel_state.delay, ChannelState};

handle_call({set_channel_delay, Delay}, _From, ChannelState) ->
            NewChannelState = ChannelState#nsime_ptp_channel_state{delay = Delay},
            {reply, ok, NewChannelState};

handle_call(get_netdevice_count, _From, ChannelState) ->
    Devices = ChannelState#nsime_ptp_channel_state.devices,
    DeviceCount = case Devices of
        {none, none} -> 0;
        {_DevicePid, none} -> 1;
        {_DevicePid1, _DevicePid2} -> 2
    end,
    {reply, DeviceCount, ChannelState};

handle_call({attach_netdevice, DevicePid}, _From, ChannelState) ->
    Devices = ChannelState#nsime_ptp_channel_state.devices,
    NewDevices = case Devices of
        {none, none} -> {DevicePid, none};
        {FirstDevicePid, none} -> {FirstDevicePid, DevicePid}
    end,
    NewChannelState = ChannelState#nsime_ptp_channel_state{devices = NewDevices},
    {reply, ok, NewChannelState};

handle_call({transmit, Packet, SourceDevicePid, TxTime}, _From, ChannelState) ->
    Devices = ChannelState#nsime_ptp_channel_state.devices,
    DestDevicePid = case Devices of 
        {SourceDevicePid, DevicePid} -> DevicePid;
        {DevicePid, SourceDevicePid} -> DevicePid
    end,
    Delay = ChannelState#nsime_ptp_channel_state.delay,
    EventTime = nsime_time:add(TxTime, Delay),
    ReceiveEvent = #nsime_event{
        time = EventTime,
        module = nsime_ptp_netdevice,
        function = receive_packet,
        arguments = [DestDevicePid, Packet],
        eventid = make_ref()
    },
    nsime_simulator:schedule(TxTime, ReceiveEvent),
    {reply, ok, ChannelState};

handle_call(terminate, _From, ChannelState) ->
    {stop, normal, stopped, ChannelState}.

handle_cast(_Request, ChannelState) ->
    {noreply, ChannelState}.

handle_info(_Request, ChannelState) ->
    {noreply, ChannelState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, ChannelState, _Extra) ->
    {ok, ChannelState}.
