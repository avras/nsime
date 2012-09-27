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
    case
    gen_server:call(DemuxPid, {lookup,
                               DestAddress,
                               DestPort,
                               SrcAddress,
                               SrcPort,
                               IncomingInterface
                              })
    of
        ipv6_not_supported ->
            erlang:error(ipv6_not_supported);
        invalid_argument ->
            erlang:error(invalid_argument);
        EndpointPid ->
            EndpointPid
    end.

simple_lookup(
    DemuxPid,
    DestAddress,
    DestPort,
    SrcAddress,
    SrcPort
) ->
    case
    gen_server:call(DemuxPid, {simple_lookup,
                               DestAddress,
                               DestPort,
                               SrcAddress,
                               SrcPort
                              })
    of
        ipv6_not_supported ->
            erlang:error(ipv6_not_supported);
        invalid_argument ->
            erlang:error(invalid_argument);
        EndpointPid ->
            EndpointPid
    end.

allocate(DemuxPid) ->
    case gen_server:call(DemuxPid, allocate, infinity) of
        ephemeral_port_allocation_failed ->
            erlang:error(ephemeral_port_allocation_failed);
        EndpointPid ->
            EndpointPid
    end.

allocate(DemuxPid, AddressOrPort) ->
    case is_integer(AddressOrPort) of
        true ->
            gen_server:call(DemuxPid, {allocate, nsime_ipv4_address:get_any(), AddressOrPort}, infinity);
        false ->
            case gen_server:call(DemuxPid, {allocate, AddressOrPort}) of
                ephemeral_port_allocation_failed ->
                    erlang:error(ephemeral_port_allocation_failed);
                EndpointPid ->
                    EndpointPid
            end
    end.

allocate(DemuxPid, Address, Port) ->
    case gen_server:call(DemuxPid, {allocate, Address, Port}, infinity) of
        duplicate_address_port ->
            erlang:error(duplicate_address_port);
        EndpointPid ->
            EndpointPid
    end.

allocate(DemuxPid, LocalAddress, LocalPort, PeerAddress, PeerPort) ->
    case gen_server:call(DemuxPid, {allocate, LocalAddress, LocalPort, PeerAddress, PeerPort}) of
        duplicate_address_port ->
            erlang:error(duplicate_address_port);
        EndpointPid ->
            EndpointPid
    end.

deallocate(DemuxPid, EndpointPid) ->
    gen_server:call(DemuxPid, {deallocate, EndpointPid}).

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
            ),
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
            {SubnetDirected, IncomingInterfaceAddress} = case MatchingInterfaceAddress of
                [] ->
                    {false, undefined};
                _ ->
                    {
                        true,
                        nsime_ipv4_interface_address:get_local_address(
                            MatchingInterfaceAddress
                        )
                    }
            end,
            IsBroadcast =
                nsime_ipv4_address:is_broadcast(DestAddress) bor SubnetDirected,
            EndpointsListWithProperties = lists:map(
                fun(E) ->
                    LocalAddress = nsime_ip_endpoint:get_local_address(E),
                    LocalAddressMatchesWildcard =
                        (LocalAddress == nsime_ipv4_address:get_any()),
                    LocalAddressMatchesDestAddress = (LocalAddress == DestAddress),
                    LocalAddressMatchesExact =
                        case (IsBroadcast band not(LocalAddressMatchesWildcard)) of
                            true ->
                                (nsime_ip_endpoint:get_local_address(E) ==
                                    IncomingInterfaceAddress);
                            false ->
                                LocalAddressMatchesDestAddress
                        end,
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
                    {
                        LocalAddressMatchesExact,
                        LocalAddressMatchesWildcard,
                        RemotePortMatchesExact,
                        RemotePortMatchesWildcard,
                        RemoteAddressMatchesExact,
                        RemoteAddressMatchesWildcard,
                        E
                    }
                end,
                RelevantEndpoints
            ),
            FilteredEndpointsListWithProperties = lists:filter(
                fun({LAME, LAMW, RPME, RPMW, RAME, RAMW, _E}) ->
                    if
                        not(LAME bor LAMW) ->
                            false;
                        not(RPME bor RPMW) ->
                            false;
                        not(RAME bor RAMW) ->
                            false;
                        true ->
                            true
                    end
                end,
                EndpointsListWithProperties
            ),
            {FinalList1, FinalList2, FinalList3, FinalList4} = lists:foldl(
                fun({LAME, LAMW, RPME, RPMW, RAME, RAMW, E}, {L1, L2, L3, L4}) ->
                    if
                        LAMW and RPMW and RAMW ->
                            {[E|L1], L2, L3, L4};
                        (LAME bor (IsBroadcast band LAMW)) and RPMW and RAMW ->
                            {L1, [E|L2], L3, L4};
                        LAMW and RPME and RAME ->
                            {L1, L2, [E|L3], L4};
                        LAME and RPME and RAME ->
                            {L1, L2, L3, [E|L4]};
                        true ->
                            {L1, L2, L3, L4}
                    end
                end,
                {[], [], [], []},
                FilteredEndpointsListWithProperties
            ),
            if
                length(FinalList4) > 0 ->
                    {reply, FinalList4, DemuxState};
                length(FinalList3) > 0 ->
                    {reply, FinalList3, DemuxState};
                length(FinalList2) > 0 ->
                    {reply, FinalList2, DemuxState};
                true ->
                    {reply, FinalList1, DemuxState}
            end;
        {8, 8} ->
            {reply, ipv6_not_supported, DemuxState};
        _ ->
            {reply, invalid_argument, DemuxState}
    end;

handle_call(
    {simple_lookup, DestAddress, DestPort, SrcAddress, SrcPort},
    _From,
    DemuxState
) ->
    case {size(DestAddress), size(SrcAddress)} of
        {4, 4} ->
            Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
            EndpointsMatchingPort = lists:filter(
                fun(E) ->
                    nsime_ip_endpoint:get_local_port(E) == DestPort
                end,
                Endpoints
            ),
            ExactMatches = lists:filter(
                fun(E) ->
                    (nsime_ip_endpoint:get_local_address(E) == DestAddress) and
                    (nsime_ip_endpoint:get_peer_port(E) == SrcPort) and
                    (nsime_ip_endpoint:get_peer_address(E) == SrcAddress)
                end,
                EndpointsMatchingPort
            ),
            case length(ExactMatches) > 0 of
                true ->
                    [E|_] = ExactMatches,
                    {reply, E, DemuxState};
                false ->
                    {E, _} = lists:foldl(
                        fun(E, {ChosenE, Genericity}) ->
                            Temp = case {
                                nsime_ip_endpoint:get_local_address(E) ==
                                    nsime_ipv4_address:get_any(),
                                nsime_ip_endpoint:get_peer_address(E) ==
                                    nsime_ipv4_address:get_any()
                            } of
                                {true, true} ->
                                    2;
                                {false, true} ->
                                    1;
                                {true, false} ->
                                    1;
                                {false, false} ->
                                    0
                            end,
                            case Temp < Genericity of
                                true ->
                                    {E, Temp};
                                false ->
                                    {ChosenE, Genericity}
                            end
                        end,
                        {undefined, 3},
                        EndpointsMatchingPort
                    ),
                    {reply, E, DemuxState}
            end;
        {8, 8} ->
            {reply, ipv6_not_supported, DemuxState};
        _ ->
            {reply, invalid_argument, DemuxState}
    end;

handle_call(allocate, _From, DemuxState) ->
    Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    case allocate_ephemeral_port(DemuxState) of
        0 ->
            {reply, ephemeral_port_allocation_failed, DemuxState};
        Port ->
            NewEndpoint =
                nsime_ip_endpoint:create(nsime_ipv4_address:get_any(), Port),
            NewDemuxState = DemuxState#nsime_ip_endpoint_demux_state{
                endpoints = [NewEndpoint | Endpoints]
            },
            {reply, NewEndpoint, NewDemuxState}
    end;

handle_call({allocate, Address}, _From, DemuxState) ->
    Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    case allocate_ephemeral_port(DemuxState) of
        0 ->
            {reply, ephemeral_port_allocation_failed, DemuxState};
        Port ->
            NewEndpoint =
                nsime_ip_endpoint:create(Address, Port),
            NewDemuxState = DemuxState#nsime_ip_endpoint_demux_state{
                endpoints = [NewEndpoint | Endpoints]
            },
            {reply, NewEndpoint, NewDemuxState}
    end;

handle_call({allocate, Address, Port}, _From, DemuxState) ->
    Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    case lists:any(
        fun(X) ->
            (nsime_ip_endpoint:get_local_address(X) == Address)
            and (nsime_ip_endpoint:get_local_port(X) == Port)
        end,
        Endpoints
    ) of
        true ->
            {reply, duplicate_address_port, DemuxState};
        false ->
            NewEndpoint = nsime_ip_endpoint:create(Address, Port),
            NewDemuxState = DemuxState#nsime_ip_endpoint_demux_state{
                endpoints = [NewEndpoint | Endpoints]
            },
            {reply, NewEndpoint, NewDemuxState}
    end;

handle_call(
    {allocate, LocalAddress, LocalPort, PeerAddress, PeerPort},
    _From,
    DemuxState
) ->
    Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    case lists:any(
        fun(E) ->
            (nsime_ip_endpoint:get_local_port(E) == LocalPort) and
            (nsime_ip_endpoint:get_local_address(E) == LocalAddress) and
            (nsime_ip_endpoint:get_peer_port(E) == PeerPort) and
            (nsime_ip_endpoint:get_peer_address(E) == PeerAddress)
        end,
        Endpoints
    ) of
        true ->
            {reply, duplicate_address_port, DemuxState};
        false ->
            NewEndpoint = nsime_ip_endpoint:create(LocalAddress, LocalPort),
            nsime_ip_endpoint:set_peer(NewEndpoint, PeerAddress, PeerPort),
            NewDemuxState = DemuxState#nsime_ip_endpoint_demux_state{
                endpoints = [NewEndpoint | Endpoints]
            },
            {reply, NewEndpoint, NewDemuxState}
    end;

handle_call({deallocate, EndpointPid}, _From, DemuxState) ->
    Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    NewEndpoints = lists:delete(EndpointPid, Endpoints),
    nsime_ip_endpoint:destroy(EndpointPid),
    NewDemuxState = DemuxState#nsime_ip_endpoint_demux_state{
        endpoints = NewEndpoints
    },
    {reply, ok, NewDemuxState};

handle_call(terminate, _From, DemuxState) ->
    Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    lists:foreach(
        fun(E) ->
            nsime_ip_endpoint:destroy(E)
        end,
        Endpoints
    ),
    {stop, normal, stopped, DemuxState}.

handle_cast(_Request, DemuxState) ->
    {noreply, DemuxState}.

handle_info(_Request, DemuxState) ->
    {noreply, DemuxState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, DemuxState, _Extra) ->
    {ok, DemuxState}.

%% Helper methods %%
allocate_ephemeral_port(DemuxState) ->
    FirstPort = DemuxState#nsime_ip_endpoint_demux_state.first_port,
    LastPort = DemuxState#nsime_ip_endpoint_demux_state.last_port,
    Count = LastPort - FirstPort,
    allocate_ephemeral_port(DemuxState, Count).

allocate_ephemeral_port(DemuxState, Count) ->
    if
        Count < 1 ->
            0;
        true ->
            EphemeralPort = DemuxState#nsime_ip_endpoint_demux_state.ephemeral_port,
            FirstPort = DemuxState#nsime_ip_endpoint_demux_state.first_port,
            LastPort = DemuxState#nsime_ip_endpoint_demux_state.last_port,
            Port = case ((EphemeralPort+1 < FirstPort) or (EphemeralPort+1 > LastPort)) of
                true ->
                    FirstPort;
                false ->
                    EphemeralPort + 1
            end,
            Endpoints = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
            case lists:any(
                fun(X) ->
                    nsime_ip_endpoint:get_local_port(X) == Port
                end,
                Endpoints
            ) of
                true ->
                    allocate_ephemeral_port(
                        DemuxState#nsime_ip_endpoint_demux_state{
                            ephemeral_port = Port
                        },
                        Count-1
                    );
                false ->
                    Port
            end
    end.
