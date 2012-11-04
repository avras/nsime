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
-include("nsime_ip_endpoint_state.hrl").
-include("nsime_ip_endpoint_demux_state.hrl").

-export([create/0, get_id/1, get_all_endpoints/1,
         lookup_port_local/2, lookup_local/3,
         lookup/6, simple_lookup/5,
         allocate/2, allocate/3, allocate/4,
         allocate/6, deallocate/2]).

create() ->
    #nsime_ip_endpoint_demux_state{
        demux_id = make_ref()
    }.

get_id(DemuxState) ->
    DemuxState#nsime_ip_endpoint_demux_state.demux_id.

get_all_endpoints(DemuxState) ->
    DemuxState#nsime_ip_endpoint_demux_state.endpoints.

lookup_port_local(DemuxState, Port) ->
    lists:any(
        fun(E) ->
            nsime_ip_endpoint:get_local_port(E) == Port
        end,
        DemuxState#nsime_ip_endpoint_demux_state.endpoints
    ).

lookup_local(DemuxState, Address, Port) ->
    lists:any(
        fun(E) ->
            (nsime_ip_endpoint:get_local_address(E) == Address)
            and (nsime_ip_endpoint:get_local_port(E) == Port)
        end,
        DemuxState#nsime_ip_endpoint_demux_state.endpoints
    ).

lookup(
    DemuxState,
    DestAddress,
    DestPort,
    SrcAddress,
    SrcPort,
    IncomingInterface
) ->
    case {size(DestAddress), size(SrcAddress)} of
        {4, 4} ->
            EndpointList = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
            DestinationDevice = nsime_ipv4_interface:get_device(IncomingInterface),
            RelevantEndpoints = lists:filter(
                fun(E) ->
                    case nsime_ip_endpoint:get_local_port(E) == DestPort of
                        false ->
                            false;
                        true ->
                            BoundNetdevice = nsime_ip_endpoint:get_bound_netdevice(E),
                            case is_pid(BoundNetdevice) of
                                false ->
                                    true;
                                true ->
                                    BoundNetdevice == DestinationDevice
                            end
                    end
                end,
                EndpointList
            ),
            AddressList = nsime_ipv4_interface:get_address_list(IncomingInterface),
            MatchingInterfaceAddress =
            case
            lists:filter(
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
            )
            of
                [] ->
                    [];
                [FirstMatch | _] ->
                    FirstMatch
            end,
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
            IsBroadcast = nsime_ipv4_address:is_broadcast(DestAddress) or SubnetDirected,
            EndpointsListWithProperties = lists:map(
                fun(E) ->
                    LocalAddress = nsime_ip_endpoint:get_local_address(E),
                    LocalAddressMatchesWildcard =
                        (LocalAddress == nsime_ipv4_address:get_any()),
                    LocalAddressMatchesDestAddress = (LocalAddress == DestAddress),
                    LocalAddressMatchesExact =
                        case (IsBroadcast and not(LocalAddressMatchesWildcard)) of
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
                        not(LAME or LAMW) ->
                            false;
                        not(RPME or RPMW) ->
                            false;
                        not(RAME or RAMW) ->
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
                        (LAME or (IsBroadcast band LAMW)) and RPMW and RAMW ->
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
                    FinalList4;
                length(FinalList3) > 0 ->
                    FinalList3;
                length(FinalList2) > 0 ->
                    FinalList2;
                true ->
                    FinalList1
            end;
        {8, 8} ->
            erlang:error(ipv6_not_supported);
        _ ->
            erlang:error(invalid_argument)
    end.

simple_lookup(
    DemuxState,
    DestAddress,
    DestPort,
    SrcAddress,
    SrcPort
) ->
    case {size(DestAddress), size(SrcAddress)} of
        {4, 4} ->
            EndpointList = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
            EndpointsMatchingPort = lists:filter(
                fun(E) ->
                    nsime_ip_endpoint:get_local_port(E) == DestPort
                end,
                EndpointList
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
                    hd(ExactMatches);
                false ->
                    {Endpoint, _} = lists:foldl(
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
                    Endpoint
            end;
        {8, 8} ->
            erlang:error(ipv6_not_supported);
        _ ->
            erlang:error(invalid_argument)
    end.

allocate(DemuxState, Callbacks) ->
    EndpointList = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    case allocate_ephemeral_port(DemuxState) of
        {0, _} ->
            erlang:error(ephemeral_port_allocation_failed);
        {Port, NewDemuxState} ->
            NewEndpoint = nsime_ip_endpoint:create(nsime_ipv4_address:get_any(), Port, Callbacks),
            NewerDemuxState = NewDemuxState#nsime_ip_endpoint_demux_state{
                endpoints = [NewEndpoint | EndpointList]
            },
            {NewEndpoint, NewerDemuxState}
    end.

allocate(DemuxState, AddressOrPort, Callbacks) ->
    case is_integer(AddressOrPort) of
        true ->
            allocate(DemuxState, nsime_ipv4_address:get_any(), AddressOrPort, Callbacks);
        false ->
            EndpointList = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
            case allocate_ephemeral_port(DemuxState) of
                {0, _} ->
                    erlang:error(ephemeral_port_allocation_failed);
                {Port, NewDemuxState} ->
                    NewEndpoint = nsime_ip_endpoint:create(AddressOrPort, Port, Callbacks),
                    NewerDemuxState = NewDemuxState#nsime_ip_endpoint_demux_state{
                        endpoints = [NewEndpoint | EndpointList]
                    },
                    {NewEndpoint, NewerDemuxState}
            end
    end.

allocate(DemuxState, Address, Port, Callbacks) ->
    EndpointList = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    case lists:any(
        fun(E) ->
            (nsime_ip_endpoint:get_local_address(E) == Address)
            and (nsime_ip_endpoint:get_local_port(E) == Port)
        end,
        EndpointList
    ) of
        true ->
            erlang:error(duplicate_address_port);
        false ->
            NewEndpoint = nsime_ip_endpoint:create(Address, Port, Callbacks),
            NewDemuxState = DemuxState#nsime_ip_endpoint_demux_state{
                endpoints = [NewEndpoint | EndpointList]
            },
            {NewEndpoint, NewDemuxState}
    end.

allocate(DemuxState, LocalAddress, LocalPort, PeerAddress, PeerPort, Callbacks) ->
    EndpointList = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    case lists:any(
        fun(E) ->
            (nsime_ip_endpoint:get_local_port(E) == LocalPort) and
            (nsime_ip_endpoint:get_local_address(E) == LocalAddress) and
            (nsime_ip_endpoint:get_peer_port(E) == PeerPort) and
            (nsime_ip_endpoint:get_peer_address(E) == PeerAddress)
        end,
        EndpointList
    ) of
        true ->
            erlang:error(duplicate_address_port);
        false ->
            NewEndpoint = nsime_ip_endpoint:create(LocalAddress, LocalPort, Callbacks),
            NewerEndpoint = nsime_ip_endpoint:set_peer(NewEndpoint, PeerAddress, PeerPort),
            NewDemuxState = DemuxState#nsime_ip_endpoint_demux_state{
                endpoints = [NewerEndpoint | EndpointList]
            },
            {NewEndpoint, NewDemuxState}
    end.

deallocate(DemuxState, Endpoint) ->
    EndpointList = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
    NewEndpointList = lists:delete(Endpoint, EndpointList),
    DemuxState#nsime_ip_endpoint_demux_state{
        endpoints = NewEndpointList
    }.

%% Helper methods %%
allocate_ephemeral_port(DemuxState) ->
    FirstPort = DemuxState#nsime_ip_endpoint_demux_state.first_port,
    LastPort = DemuxState#nsime_ip_endpoint_demux_state.last_port,
    Count = LastPort - FirstPort,
    allocate_ephemeral_port(DemuxState, Count).

allocate_ephemeral_port(DemuxState, Count) ->
    if
        Count < 1 ->
            {0, DemuxState};
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
            EndpointList = DemuxState#nsime_ip_endpoint_demux_state.endpoints,
            case lists:any(
                fun(E) ->
                    nsime_ip_endpoint:get_local_port(E) == Port
                end,
                EndpointList
            ) of
                true ->
                    allocate_ephemeral_port(
                        DemuxState#nsime_ip_endpoint_demux_state{
                            ephemeral_port = Port
                        },
                        Count-1
                    );
                false ->
                    {Port, DemuxState}
            end
    end.
