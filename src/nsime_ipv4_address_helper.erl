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

%% Purpose : IPv4 address helper module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_address_helper).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_ipv4_address_helper_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, set_base/3, set_base/4, new_network/1,
         new_address/1, assign/2]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(HelperPid) ->
    gen_server:call(HelperPid, terminate).

set_base(HelperPid, NetworkAddress, Mask) ->
    gen_server:call(HelperPid, {set_base, NetworkAddress, Mask, "0.0.0.1"}).

set_base(HelperPid, NetworkAddress, Mask, BaseAddress) ->
    gen_server:call(HelperPid, {set_base, NetworkAddress, Mask, BaseAddress}).

new_network(HelperPid) ->
    gen_server:call(HelperPid, new_network).

new_address(HelperPid) ->
    case gen_server:call(HelperPid, new_address) of
        nsime_address_overflow ->
            erlang:error(nsime_address_overflow);
        Address ->
            Address
    end.

assign(HelperPid, DevicePidList) ->
    case gen_server:call(HelperPid, {assign, DevicePidList}) of
        nsime_address_overflow ->
            erlang:error(nsime_address_overflow);
        InterfacePidList ->
            InterfacePidList
    end.

init([]) ->
    HelperState = #nsime_ipv4_address_helper_state{},
    {ok, HelperState}.

handle_call({set_base, NetworkAddress, Mask, BaseAddress}, _From, HelperState) ->
    {ok, NetworkAddressTuple} = inet_parse:address(NetworkAddress),
    {ok, MaskTuple} = inet_parse:address(Mask),
    {ok, BaseAddressTuple} = inet_parse:address(BaseAddress),
    AddressBits = 32 - nsime_ipv4_mask:get_prefix_length(MaskTuple),
    NewHelperState = HelperState#nsime_ipv4_address_helper_state{
        network = NetworkAddressTuple,
        mask = MaskTuple,
        address = BaseAddressTuple,
        base = BaseAddressTuple,
        max = (1 bsl AddressBits) - 2
    },
    {reply, ok, NewHelperState};

handle_call(new_network, _From, HelperState) ->
    Mask = HelperState#nsime_ipv4_address_helper_state.mask,
    AddressBits = 32 - nsime_ipv4_mask:get_prefix_length(Mask),
    NetworkAddress = HelperState#nsime_ipv4_address_helper_state.network,
    <<NetworkInteger:32>> = list_to_binary(tuple_to_list(NetworkAddress)),
    ShiftedIncrNetworkInteger = (NetworkInteger bsr AddressBits) + 1,
    NewNetworkAddress = list_to_tuple(
        binary_to_list(
            <<(ShiftedIncrNetworkInteger bsl AddressBits):32>>
        )
    ),
    BaseAddressTuple = HelperState#nsime_ipv4_address_helper_state.base,
    NewHelperState = HelperState#nsime_ipv4_address_helper_state{
        network = NewNetworkAddress,
        address = BaseAddressTuple
    },
    {reply, NewNetworkAddress, NewHelperState};

handle_call(new_address, _From, HelperState) ->
    CurrentAddress = HelperState#nsime_ipv4_address_helper_state.address,
    MaxAdresses = HelperState#nsime_ipv4_address_helper_state.max,
    <<CurrentAddressInteger:32>> = list_to_binary(tuple_to_list(CurrentAddress)),
    case CurrentAddressInteger =< MaxAdresses of
        false ->
            {reply, nsime_address_overflow, HelperState};
        true ->
            NetworkAddress = HelperState#nsime_ipv4_address_helper_state.network,
            <<NetworkInteger:32>> = list_to_binary(tuple_to_list(NetworkAddress)),
            Address = list_to_tuple(
                binary_to_list(
                    <<(NetworkInteger bor CurrentAddressInteger):32>>
                )
            ),
            NewAddress = list_to_tuple(
                binary_to_list(
                    <<(NetworkInteger bor (CurrentAddressInteger+1)):32>>
                )
            ),
            NewHelperState = HelperState#nsime_ipv4_address_helper_state{
                address = NewAddress
            },
            {reply, Address, NewHelperState}
    end;

handle_call({assign, DevicePidList}, _From, HelperState) ->
    CurrentAddress = HelperState#nsime_ipv4_address_helper_state.address,
    MaxAdresses = HelperState#nsime_ipv4_address_helper_state.max,
    <<CurrentAddressInteger:32>> = list_to_binary(tuple_to_list(CurrentAddress)),
    case (CurrentAddressInteger+length(DevicePidList) - 1) =< MaxAdresses of
        false ->
            {reply, nsime_address_overflow, HelperState};
        true ->
            Mask = HelperState#nsime_ipv4_address_helper_state.mask,
            NetworkAddress = HelperState#nsime_ipv4_address_helper_state.network,
            <<NetworkInteger:32>> = list_to_binary(tuple_to_list(NetworkAddress)),
            DevicesAndIndices = lists:zip(DevicePidList, lists:seq(1, length(DevicePidList))),
            InterfacePidList =
            lists:map(
                fun({DevicePid, Index})->
                    NodePid = nsime_netdevice:get_node(DevicePid),
                    Ipv4ProtocolPid = nsime_node:get_object(NodePid, nsime_ipv4_protocol),
                    ExistingInterfacePid = nsime_ipv4_protocol:get_interface_for_device(
                        Ipv4ProtocolPid,
                        DevicePid
                    ),
                    InterfacePid =
                    case is_pid(ExistingInterfacePid) of
                        true ->
                            ExistingInterfacePid;
                        false ->
                            nsime_ipv4_protocol:add_interface(Ipv4ProtocolPid, DevicePid)
                    end,
                    NewAddress = list_to_tuple(
                        binary_to_list(
                            <<(NetworkInteger bor (CurrentAddressInteger+Index-1)):32>>
                        )
                    ),
                   InterfaceAddress = nsime_ipv4_interface_address:create(NewAddress, Mask),
                   nsime_ipv4_interface:add_address(InterfacePid, InterfaceAddress),
                   nsime_ipv4_interface:set_metric(InterfacePid, 1),
                   nsime_ipv4_protocol:set_up(Ipv4ProtocolPid, InterfacePid),
                   InterfacePid
                end,
                DevicesAndIndices
            ),
            LatestAddress = list_to_tuple(
                binary_to_list(
                    <<(NetworkInteger bor (CurrentAddressInteger+length(DevicePidList))):32>>
                )
            ),
            NewHelperState = HelperState#nsime_ipv4_address_helper_state{
                address = LatestAddress
            },
            {reply, InterfacePidList, NewHelperState}
    end;

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
