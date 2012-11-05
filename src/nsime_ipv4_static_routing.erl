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

%% Purpose : IPv4 static routing protocol module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_static_routing).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_ipv4_route.hrl").
-include("nsime_ipv4_routing_table_entry.hrl").
-include("nsime_ipv4_static_routing_state.hrl").

-export([create/0, route_input/10, route_input/11, route_output/4,
         notify_interface_up/3, notify_interface_down/3, notify_add_address/4,
         notify_remove_address/4, populate_network_routes/2, add_network_route/5,
         add_network_route/6, add_host_route/4, add_host_route/5,
         set_default_route/4, get_network_routes/1]).

create() ->
    {nsime_ipv4_static_routing, #nsime_ipv4_static_routing_state{}}.

route_input(
    RoutingState,
    Packet,
    Ipv4Header,
    _IngressNetdevice,
    Interface,
    UnicastForwardCallback,
    _MulticastForwardCallback,
    LocalDeliverCallback,
    ErrorCallback,
    InterfaceList,
    _WeakEsModel
) ->
    DestinationAddress = nsime_ipv4_header:get_destination_address(Ipv4Header),
    case
        nsime_ipv4_address:is_multicast(DestinationAddress) or
        nsime_ipv4_address:is_broadcast(DestinationAddress)
    of
        true ->
            options_not_supported;
        false ->
            case
                lists:foldl(
                    fun(I, MatchingAddresses) ->
                        AddressList = nsime_ipv4_interface:get_address_list(I),
                        NewAddressList = lists:filter(
                            fun(A) ->
                                (nsime_ipv4_interface_address:get_local_address(A) == DestinationAddress)
                                or
                                (nsime_ipv4_interface_address:get_broadcast_address(A) == DestinationAddress)
                            end,
                            AddressList
                        ),
                        lists:flatten([NewAddressList | MatchingAddresses])
                    end,
                    [],
                    InterfaceList
                )
            of
                [] ->
                    case nsime_ipv4_interface:is_forwarding(Interface) of
                        false ->
                            {Mod, Fun, Args} = ErrorCallback,
                            NewArgs = lists:flatten([Args, [Packet, Ipv4Header, error_noroutetohost, self(), undefined]]),
                            erlang:apply(Mod, Fun, NewArgs),
                            false;
                        true ->
                            Route = lookup_static(DestinationAddress, undefined, RoutingState, InterfaceList),
                            case Route of
                                undefined ->
                                    false;
                                _ ->
                                    nsime_callback:apply(
                                        UnicastForwardCallback,
                                        [Route, Packet, Ipv4Header]
                                    ),
                                    true
                            end
                    end;
                _ ->
                    case LocalDeliverCallback of
                        undefined ->
                            false;
                        _ ->
                            nsime_callback:apply(
                                LocalDeliverCallback,
                                [Packet, Ipv4Header, Interface]
                            ),
                            true
                    end

            end
    end.

route_input(
    RoutingState,
    Packet,
    Ipv4Header,
    IngressNetdevice,
    InterfaceId,
    UnicastForwardCallback,
    MulticastForwardCallback,
    LocalDeliverCallback,
    ErrorCallback,
    InterfaceList
) ->
    route_input(
        RoutingState,
        Packet,
        Ipv4Header,
        IngressNetdevice,
        InterfaceId,
        UnicastForwardCallback,
        MulticastForwardCallback,
        LocalDeliverCallback,
        ErrorCallback,
        InterfaceList,
        true
    ).

route_output(
    RoutingState,
    Ipv4Header,
    OutputNetdevice,
    InterfaceList
) ->
    DestinationAddress = nsime_ipv4_header:get_destination_address(Ipv4Header),
    Route = lookup_static(DestinationAddress, OutputNetdevice, RoutingState, InterfaceList),
    case Route of
        undefined ->
            {error_noroutetohost, undefined};
        _ ->
            {error_noterror, Route}
    end.

notify_interface_up(RoutingState, InterfaceId, InterfaceList) ->
    do_notify_interface_up(InterfaceId, RoutingState, InterfaceList).

notify_interface_down(RoutingState, InterfaceId, InterfaceList) ->
    do_notify_interface_down(InterfaceId, RoutingState, InterfaceList).

notify_add_address(RoutingState, InterfaceId, InterfaceAddress, InterfaceList) ->
    [Interface] = lists:filter(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    case nsime_ipv4_interface:is_up(Interface) of
        false ->
            RoutingState;
        true ->
            case
                (nsime_ipv4_interface_address:get_local_address(InterfaceAddress) =/= undefined) and
                (nsime_ipv4_interface_address:get_mask(InterfaceAddress) =/= undefined)
            of
                false ->
                    RoutingState;
                true ->
                    NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
                    Route = nsime_ipv4_routing_table_entry:create_network_route(
                          nsime_ipv4_address:combine_mask(
                              nsime_ipv4_interface_address:get_local_address(
                                  InterfaceAddress
                              ),
                              nsime_ipv4_interface_address:get_mask(InterfaceAddress)
                          ),
                          nsime_ipv4_interface_address:get_mask(InterfaceAddress),
                          InterfaceId,
                          0
                    ),
                    NewNetworkRoutes = [Route | NetworkRoutes],
                    RoutingState#nsime_ipv4_static_routing_state{
                        network_routes = NewNetworkRoutes
                    }
            end
    end.

notify_remove_address(RoutingState, InterfaceId, InterfaceAddress, InterfaceList) ->
    [Interface] = lists:filter(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    case nsime_ipv4_interface:is_up(Interface) of
        false ->
            RoutingState;
        true ->
            NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
            NewNetworkRoutes = lists:filter(
                fun(R) ->
                    NetworkMask = nsime_ipv4_interface_address:get_mask(InterfaceAddress),
                    NetworkAddress = nsime_ipv4_address:combine_mask(
                        nsime_ipv4_interface_address:get_local_address(
                            InterfaceAddress
                        ),
                        NetworkMask
                    ),
                    case
                        (R#nsime_ipv4_routing_table_entry.interface == InterfaceId) and
                        (nsime_ipv4_routing_table_entry:is_network(R)) and
                        (R#nsime_ipv4_routing_table_entry.destination == NetworkAddress) and
                        (R#nsime_ipv4_routing_table_entry.network_mask == NetworkMask)
                    of
                        true ->
                            false;
                        false ->
                            true
                    end
                end,
                NetworkRoutes
            ),
            RoutingState#nsime_ipv4_static_routing_state{
                network_routes = NewNetworkRoutes
            }
    end.

populate_network_routes(RoutingState, InterfaceList) ->
    lists:foldl(
        fun(I, State) ->
            case nsime_ipv4_interface:is_up(I) of
                false ->
                    do_notify_interface_up(I, State);
                true ->
                    do_notify_interface_down(I, State)
            end
        end,
        RoutingState,
        InterfaceList
    ).

add_network_route(
    RoutingState,
    NetworkAddress,
    NetworkMask,
    InterfaceId,
    Metric
) ->
    NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
    Route = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        NetworkMask,
        InterfaceId,
        Metric
    ),
    NewNetworkRoutes = [Route | NetworkRoutes],
    RoutingState#nsime_ipv4_static_routing_state{
        network_routes = NewNetworkRoutes
    }.

add_network_route(
    RoutingState,
    NetworkAddress,
    NetworkMask,
    NextHopAddress,
    InterfaceId,
    Metric
) ->
    NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
    Route = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        NetworkMask,
        NextHopAddress,
        InterfaceId,
        Metric
    ),
    NewNetworkRoutes = [Route | NetworkRoutes],
    RoutingState#nsime_ipv4_static_routing_state{
        network_routes = NewNetworkRoutes
    }.

add_host_route(RoutingState, DestinationAddress, InterfaceId, Metric) ->
    add_network_route(
        RoutingState, 
        DestinationAddress,
        nsime_ipv4_mask:get_ones(),
        InterfaceId,
        Metric
    ).

add_host_route(
    RoutingState,
    DestinationAddress,
    NextHopAddress,
    InterfaceId,
    Metric
) ->
    add_network_route(
        RoutingState, 
        DestinationAddress,
        nsime_ipv4_mask:get_ones(),
        NextHopAddress,
        InterfaceId,
        Metric
    ).

set_default_route(RoutingState, NextHopAddress, InterfaceId, Metric) ->
    add_network_route(
        RoutingState,
        nsime_ipv4_address:get_zero(),
        nsime_ipv4_mask:get_zero(),
        NextHopAddress,
        InterfaceId,
        Metric
    ).

get_network_routes(RoutingState) ->
    RoutingState#nsime_ipv4_static_routing_state.network_routes.

%% Helper methods %%

lookup_static(DestinationAddress, OutputNetdevice, RoutingState, InterfaceList) ->
    case nsime_ipv4_address:is_local_multicast(DestinationAddress) of
        true ->
            InterfaceId = nsime_netdevice:get_interface(OutputNetdevice),
            [Interface] = lists:filter(
                fun(I) ->
                    nsime_ipv4_interface:get_id(I) == InterfaceId
                end,
                InterfaceList
            ),
            FirstAddress = hd(nsime_ipv4_interface:get_address_list(Interface)),
            SrcAddress = nsime_ipv4_interface_address:get_local_address(FirstAddress),
            #nsime_ipv4_route{
                destination = DestinationAddress,
                source = SrcAddress,
                gateway = nsime_ipv4_address:get_zero(),
                output_device = OutputNetdevice
            };
        false ->
            NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
            {BestMatchRoute, _, _} = lists:foldl(
                fun(R, {BestRoute, BestMaskLength, ShortestMetric}) ->
                    Mask = R#nsime_ipv4_routing_table_entry.network_mask,
                    Metric = R#nsime_ipv4_routing_table_entry.metric,
                    MaskLength = nsime_ipv4_mask:get_prefix_length(Mask),
                    CandidateAddress = R#nsime_ipv4_routing_table_entry.destination,
                    case (nsime_ipv4_address:combine_mask(DestinationAddress, Mask) ==
                          nsime_ipv4_address:combine_mask(CandidateAddress, Mask))
                    of
                        false ->
                            {BestRoute, BestMaskLength, ShortestMetric};
                        true ->
                            [Interface] = lists:filter(
                                fun(I) ->
                                    nsime_ipv4_interface:get_id(I) == R#nsime_ipv4_routing_table_entry.interface
                                end,
                                InterfaceList
                            ),
                            case is_pid(OutputNetdevice) and (OutputNetdevice =/= nsime_ipv4_interface:get_device(Interface)) of
                                true ->
                                    {BestRoute, BestMaskLength, ShortestMetric};
                                false ->
                                    case MaskLength < BestMaskLength of
                                        true ->
                                            {BestRoute, BestMaskLength, ShortestMetric};
                                        false ->
                                            NewShortestMetric =
                                            case MaskLength > BestMaskLength of
                                                true ->
                                                    infinity;
                                                false ->
                                                    ShortestMetric
                                            end,
                                            NewBestMaskLength = MaskLength,
                                            case Metric > NewShortestMetric of
                                                true ->
                                                    {BestRoute, NewBestMaskLength, NewShortestMetric};
                                                false ->
                                                    SrcAddress = source_address_selection(
                                                        Interface,
                                                        CandidateAddress
                                                    ),
                                                    NewBestRoute = #nsime_ipv4_route{
                                                        destination = CandidateAddress,
                                                        source = SrcAddress,
                                                        gateway = R#nsime_ipv4_routing_table_entry.gateway,
                                                        output_device = nsime_ipv4_interface:get_device(Interface)
                                                    },
                                                    {NewBestRoute, NewBestMaskLength, Metric}
                                            end
                                    end
                            end
                    end
                end,
                {undefined, 0, infinity},
                NetworkRoutes
            ),
            BestMatchRoute
    end.

source_address_selection(Interface, Address) ->
    case nsime_ipv4_interface:get_address_list(Interface) of
        [] ->
            throw(error_noroutetohost);
        InterfaceAddressList ->
            CandidateAddress = nsime_ipv4_interface_address:get_local_address(hd(InterfaceAddressList)),
            MatchingInterfaceAddresses = lists:filter(
                fun(A) ->
                    Mask = nsime_ipv4_interface_address:get_mask(A),
                    case
                        nsime_ipv4_address:combine_mask(
                            nsime_ipv4_interface_address:get_local_address(A),
                            Mask
                        ) ==
                        nsime_ipv4_address:combine_mask(
                            Address,
                            Mask
                        )
                    of
                        true ->
                            case nsime_ipv4_interface_address:is_secondary(A) of
                                false ->
                                    true;
                                true ->
                                    false
                            end;
                        false ->
                            false
                    end
                end,
                InterfaceAddressList
            ),
            case length(MatchingInterfaceAddresses) > 0 of
                false ->
                    CandidateAddress;
                true ->
                    nsime_ipv4_interface_address:get_local_address(hd(MatchingInterfaceAddresses))
            end
    end.

do_notify_interface_up(InterfaceId, RoutingState, InterfaceList) ->
    [Interface] = lists:filter(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    do_notify_interface_up(Interface, RoutingState).

do_notify_interface_up(Interface, RoutingState) ->
    AddressList = nsime_ipv4_interface:get_address_list(Interface),
    NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
    NewRoutes = lists:foldl(
        fun(A, CurrentRoutes) ->
            case
                (nsime_ipv4_interface_address:get_local_address(A) =/= undefined) and
                (nsime_ipv4_interface_address:get_mask(A) =/= undefined) and
                (nsime_ipv4_interface_address:get_mask(A) =/= nsime_ipv4_mask:get_ones())
            of
                true ->
                    Route = nsime_ipv4_routing_table_entry:create_network_route(
                        nsime_ipv4_address:combine_mask(
                            nsime_ipv4_interface_address:get_local_address(A),
                            nsime_ipv4_interface_address:get_mask(A)
                        ),
                        nsime_ipv4_interface_address:get_mask(A),
                        nsime_ipv4_interface:get_id(Interface),
                        0
                    ),
                    [Route | CurrentRoutes];
                false ->
                    CurrentRoutes
            end
        end,
        [],
        AddressList
    ),
    NewNetworkRoutes = lists:flatten([NewRoutes | NetworkRoutes]),
    RoutingState#nsime_ipv4_static_routing_state{
        network_routes = NewNetworkRoutes
    }.

do_notify_interface_down(InterfaceId, RoutingState, InterfaceList) ->
    [Interface] = lists:filter(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    do_notify_interface_down(Interface, RoutingState).

do_notify_interface_down(Interface, RoutingState) ->
    NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
    NewNetworkRoutes = lists:filter(
        fun(R) ->
            case R#nsime_ipv4_routing_table_entry.interface == nsime_ipv4_interface:get_id(Interface) of
                true ->
                    false;
                false ->
                    true
            end
        end,
        NetworkRoutes
    ),
    RoutingState#nsime_ipv4_static_routing_state{
        network_routes = NewNetworkRoutes
    }.
