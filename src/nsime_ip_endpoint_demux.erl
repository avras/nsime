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

%% Purpose : IP endpoint demultiplexer module
%% Author : Saravanan Vijayakumaran

-module(nsime_ip_endpoint_demux).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_ip_endpoint_demux_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, get_all_endpoints/1,
         lookup_port_local/2, lookup_local/3,
         lookup/6, simple_lookup/5,
         allocate/1, allocate/2, allocate/3,
         allocate/5, deallocate/2]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(DemuxPid) ->
    gen_server:call(DemuxPid, terminate).

get_all_endpoints(DemuxPid) ->
    gen_server:call(DemuxPid, get_all_endpoints).

lookup_port_local(DemuxPid, Port) ->
    gen_server:call(DemuxPid, {lookup_port_local, Port}).

lookup_local(DemuxPid, Address, Port) ->
    gen_server:call(DemuxPid, {lookup_local, Address, Port}).

lookup(
    DemuxPid,
    DestAddress,
    DestPort,
    SrcAddress,
    SrcPort,
    IncomingInterface
) ->
    gen_server:call(DemuxPid, {lookup,
                               DestAddress,
                               DestPort,
                               SrcAddress,
                               SrcPort,
                               IncomingInterface
                              }).

init([]) ->
    DemuxState = #nsime_ip_endpoint_demux_state{},
    {ok, DemuxState}.

handle_call(get_all_endpoints, _From, DemuxState) ->
    Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    {reply, Endpoints, DemuxState};

handle_call({lookup_port_local, Port}, _From, DemuxState) ->
    Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    {
        reply,
        lists:any(
            fun(X) ->
                nsime_ip_endpoint:get_local_port(X) == Port
            end,
            Endpoints
        ),
        DemuxState
    };

handle_call({lookup_local, Address, Port}, _From, DemuxState) ->
    Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    {
        reply,
        lists:any(
            fun(X) ->
                (nsime_ip_endpoint:get_local_address(X) == Address)
                and (nsime_ip_endpoint:get_local_port(X) == Port)
            end,
            Endpoints
        ),
        DemuxState
    };

handle_call(
    {lookup, DestAddress, DestPort, SrcAddress, SrcPort, IncomingInterface},
    _From,
    DemuxState
) ->
    case {size(DestAddress), size(SrcAddress)} of
        {4, 4} ->
            Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
            DestinationDevice = nsime_ip_interface:get_netdevice(IncomingInterface),
            RelevantEndpoints = lists:filter(
                fun(X) ->
                    (nsime_ip_endpoint:get_local_port(X) == DestPort) and
                    (nsime_ip_endpoint:get_bound_netdevice(X) == DestinationDevice)
                end,
                Endpoints
            )
            AddressList = nsime_ipv4_interface:get_address_list(IncomingInterface),
            [MatchingInterfaceAddress | _] = lists:filter(
                fun(A) ->
                    Address = nsime_ipv4_interface_address:get_local_address(A),
                    Mask = nsime_ipv4_interface_address:get_mask(A),
                    (
                        nsime_ipv4_address:combine_mask(Address, Mask) ==
                        nsime_ipv4_address:combine_mask(DestAddress, Mask)
                    ) and
                    nsime_ipv4_address:is_subnet_directed_broadcast(DestAddress, Mask)
                end,
                AddressList
            ),
            case MatchingInterfaceAddress of
                [] ->
                    SubnetDirected = false;
                _ ->
                    SubnetDirected = true,
                    IncomingInterfaceAddress =
                        nsime_ipv4_interface_address:get_local_address(
                            MatchingInterfaceAddress
                        )
            end,
            IsBroadcast =
                nsime_ipv4_address:is_broadcast(DestAddress) bor SubnetDirected,
            EndpointsListWithProperties = lists:map(
                fun(E) ->
                    LocalAddressMatchesWildcard =
                        (
                            nsime_ip_endpoint:get_local_address(E) ==
                            nsime_ipv4_address:get_any()
                        ),
                    LocalAddressMatchesDestAddress =
                        (nsime_ip_endpoint:get_local_address(E) == DestAddress),
                    LocalAddressMatchesExact =
                    case (IsBroadcast band not(LocalAddressMatchesDestAddress)) of
                        true ->
                            nsime_ip_endpoint:get_local_address(E) ==
                                IncomingInterfaceAddress;
                        false ->
                            false
                    end,
                    case (LocalAddressMatchesExact bor LocalAddressMatchesWildcard) of
                        false ->
                            false;
                        true ->
                            RemotePortMatchesExact =
                                (nsime_ip_endpoint:get_peer_port(E) == SrcPort),
                            RemotePortMatchesWildcard =
                                (nsime_ip_endpoint:get_peer_port(E) == 0),
                            RemoteAddressMatchesExact =
                                (nsime_ip_endpoint:get_peer_address(E) == SrcAddress),
                            RemoteAddressMatchesWildcard =
                                (
                                    nsime_ip_endpoint:get_peer_address(E) ==
                                    nsime_ipv4_address:get_any()
                                ),
                            case {
                                RemotePortMatchesExact bor RemotePortMatchesWildcard,
                                RemoteAddressMatchesExact bor RemoteAddressMatchesWildcard
                            } of
                                {true, true} ->
                                    true;
                                {_, _} ->
                                    false
                            end
                    end
                end,
                RelevantEndpoints
            ),
            {reply, FinalEndpointsList, DemuxState};
        {6, 6} ->
            erlang:error(ipv6_not_supported);
        _ ->
            erlang:error(invalid_argument)
    end;


handle_call(terminate, _From, DemuxState) ->
    {stop, normal, stopped, DemuxState}.

handle_cast(_Request, DemuxState) ->
    {noreply, DemuxState}.

handle_info(_Request, DemuxState) ->
    {noreply, DemuxState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, DemuxState, _Extra) ->
    {ok, DemuxState}.
