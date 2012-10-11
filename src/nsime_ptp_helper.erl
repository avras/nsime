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

%% Purpose : Point-to-point helper module
%% Author : Saravanan Vijayakumaran

-module(nsime_ptp_helper).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_ptp_helper_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, call_on_device/3, call_on_channel/3,
         install/2]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(HelperPid) ->
    gen_server:call(HelperPid, terminate).

call_on_device(HelperPid, Function, Arguments) ->
    gen_server:call(HelperPid, {call_on_device, Function, Arguments}).

call_on_channel(HelperPid, Function, Arguments) ->
    gen_server:call(HelperPid, {call_on_channel, Function, Arguments}).

install(HelperPid, NodePidList) ->
    case {is_list(NodePidList), length(NodePidList)} of
        {true, 2} ->
            gen_server:call(HelperPid, {install, NodePidList});
        _ ->
            erlang:error(invalid_argument)
    end.

init([]) ->
    HelperState = #nsime_ptp_helper_state{},
    {ok, HelperState}.

handle_call({call_on_device, Function, Arguments}, _From, HelperState) ->
    DeviceCalls = HelperState#nsime_ptp_helper_state.device_calls,
    NewHelperState = HelperState#nsime_ptp_helper_state{
        device_calls = [{nsime_ptp_netdevice, Function, [Arguments]} | DeviceCalls]
    },
    {reply, ok, NewHelperState};

handle_call({call_on_channel, Function, Arguments}, _From, HelperState) ->
    ChannelCalls = HelperState#nsime_ptp_helper_state.channel_calls,
    NewHelperState = HelperState#nsime_ptp_helper_state{
        channel_calls = [{nsime_ptp_channel, Function, [Arguments]} | ChannelCalls]
    },
    {reply, ok, NewHelperState};

handle_call({install, [NodePid1, NodePid2]}, _From, HelperState) ->
    DeviceCalls = HelperState#nsime_ptp_helper_state.device_calls,
    ChannelCalls = HelperState#nsime_ptp_helper_state.channel_calls,
    DevicePid1 = nsime_ptp_netdevice:create(),
    nsime_ptp_netdevice:set_address(DevicePid1, nsime_mac_address:allocate()),
    nsime_node:add_netdevice(NodePid1, DevicePid1),

    DevicePid2 = nsime_ptp_netdevice:create(),
    nsime_ptp_netdevice:set_address(DevicePid2, nsime_mac_address:allocate()),
    nsime_node:add_netdevice(NodePid2, DevicePid2),

    lists:foreach(
        fun(D) ->
            lists:foreach(
                fun({Mod, Fun, Args}) ->
                    erlang:apply(Mod, Fun, [D | Args])
                end,
                DeviceCalls
            )
        end,
        [DevicePid1, DevicePid2]
    ),
    ChannelPid = nsime_ptp_channel:create(),
    lists:foreach(
        fun({Mod, Fun, Args}) ->
            erlang:apply(Mod, Fun, [ChannelPid | Args])
        end,
        ChannelCalls
    ),

    nsime_ptp_netdevice:attach_channel(DevicePid1, ChannelPid),
    nsime_ptp_netdevice:attach_channel(DevicePid2, ChannelPid),
    {reply, [DevicePid1, DevicePid2], HelperState};

handle_call(terminate, _From, HelperState) ->
    {stop, normal, stopped, HelperState}.

handle_cast(_Request, HelperState) ->
    {noreply, HelperState}.

handle_info(_Request, HelperState) ->
    {noreply, HelperState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, HelperState, _Extra) ->
    {ok, HelperState}.
