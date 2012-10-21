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

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, route_input/10, route_input/11, route_output/4,
         notify_interface_up/2, notify_interface_down/2, notify_add_address/3,
         notify_remove_address/3, set_ipv4_protocol/3, add_network_route/5,
         add_network_route/6, add_host_route/4, add_host_route/5,
         set_default_route/4, get_network_routes/1]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(RoutingPid) ->
    gen_server:call(RoutingPid, terminate).

route_input(
    RoutingPid,
    Packet,
    Ipv4Header,
    IngressNetdevice,
    InterfacePid,
    UnicastForwardCallback,
    MulticastForwardCallback,
    LocalDeliverCallback,
    ErrorCallback,
    InterfaceList,
    WeakEsModel
) ->
    case
    gen_server:call(RoutingPid, {route_input,
                                 Packet,
                                 Ipv4Header,
                                 IngressNetdevice,
                                 InterfacePid,
                                 UnicastForwardCallback,
                                 MulticastForwardCallback,
                                 LocalDeliverCallback,
                                 ErrorCallback,
                                 InterfaceList,
                                 WeakEsModel
                                })
    of
        options_not_supported ->
            throw(options_not_supported);
        false ->
            false;
        true ->
            true
    end.

route_input(
    RoutingPid,
    Packet,
    Ipv4Header,
    IngressNetdevice,
    InterfacePid,
    UnicastForwardCallback,
    MulticastForwardCallback,
    LocalDeliverCallback,
    ErrorCallback,
    InterfaceList
) ->
    route_input(
        RoutingPid,
        Packet,
        Ipv4Header,
        IngressNetdevice,
        InterfacePid,
        UnicastForwardCallback,
        MulticastForwardCallback,
        LocalDeliverCallback,
        ErrorCallback,
        InterfaceList,
        true
    ).

route_output(
    RoutingPid,
    Packet,
    Ipv4Header,
    OutputNetdevice
) ->
    gen_server:call(RoutingPid, {route_output,
                                 Packet,
                                 Ipv4Header,
                                 OutputNetdevice
                                }).

notify_interface_up(RoutingPid, InterfacePid) ->
    gen_server:call(RoutingPid, {notify_interface_up, InterfacePid}).

notify_interface_down(RoutingPid, InterfacePid) ->
    gen_server:call(RoutingPid, {notify_interface_down, InterfacePid}).

notify_add_address(RoutingPid, InterfacePid, InterfaceAddress) ->
    gen_server:call(RoutingPid, {notify_add_address,
                                 InterfacePid,
                                 InterfaceAddress
                                }).

notify_remove_address(RoutingPid, InterfacePid, InterfaceAddress) ->
    gen_server:call(RoutingPid, {notify_remove_address,
                                 InterfacePid,
                                 InterfaceAddress
                                }).

set_ipv4_protocol(RoutingPid, Ipv4ProtocolPid, InterfaceList) ->
    gen_server:call(RoutingPid, {set_ipv4_protocol, Ipv4ProtocolPid, InterfaceList}).

add_network_route(
    RoutingPid,
    NetworkAddress,
    NetworkMask,
    InterfacePid,
    Metric
) ->
    gen_server:call(RoutingPid, {add_network_route,
                                 NetworkAddress,
                                 NetworkMask,
                                 InterfacePid,
                                 Metric
                                }).

add_network_route(
    RoutingPid,
    NetworkAddress,
    NetworkMask,
    NextHopAddress,
    InterfacePid,
    Metric
) ->
    gen_server:call(RoutingPid, {add_network_route,
                                 NetworkAddress,
                                 NetworkMask,
                                 NextHopAddress,
                                 InterfacePid,
                                 Metric
                                }).

add_host_route(RoutingPid, DestinationAddress, InterfacePid, Metric) ->
    add_network_route(
        RoutingPid, 
        DestinationAddress,
        nsime_ipv4_mask:get_ones(),
        InterfacePid,
        Metric
    ).

add_host_route(
    RoutingPid,
    DestinationAddress,
    NextHopAddress,
    InterfacePid,
    Metric
) ->
    add_network_route(
        RoutingPid, 
        DestinationAddress,
        nsime_ipv4_mask:get_ones(),
        NextHopAddress,
        InterfacePid,
        Metric
    ).

set_default_route(RoutingPid, NextHopAddress, InterfacePid, Metric) ->
    add_network_route(
        RoutingPid,
        nsime_ipv4_address:get_zero(),
        nsime_ipv4_mask:get_zero(),
        NextHopAddress,
        InterfacePid,
        Metric
    ).

get_network_routes(RoutingPid) ->
    gen_server:call(RoutingPid, get_network_routes).

init([]) ->
    RoutingState = #nsime_ipv4_static_routing_state{},
    {ok, RoutingState}.

handle_call(
    {
        route_input,
        Packet,
        Ipv4Header,
        _IngressNetdevice,
        InterfacePid,
        UnicastForwardCallback,
        _MulticastForwardCallback,
        LocalDeliverCallback,
        ErrorCallback,
        InterfaceList,
        _WeakEsModel
    },
    _From,
    RoutingState
) ->
    DestinationAddress = nsime_ipv4_header:get_destination_address(Ipv4Header),
    case
        nsime_ipv4_address:is_multicast(DestinationAddress) or
        nsime_ipv4_address:is_broadcast(DestinationAddress)
    of
        true ->
            {reply, options_not_supported, RoutingState};
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
                    case nsime_ipv4_interface:is_forwarding(InterfacePid) of
                        false ->
                            {Mod, Fun, Args} = ErrorCallback,
                            NewArgs = lists:flatten([Args, [Packet, Ipv4Header, error_noroutetohost, self(), undefined]]),
                            erlang:apply(Mod, Fun, NewArgs),
                            {reply, false, RoutingState};
                        true ->
                            Route = lookup_static(DestinationAddress, undefined, RoutingState),
                            case Route of
                                undefined ->
                                    {reply, false, RoutingState};
                                _ ->
                                    {Mod, Fun, Args} = UnicastForwardCallback,
                                    NewArgs = lists:flatten([Args, [Route, Packet, Ipv4Header]]),
                                    Event = #nsime_event{
                                        module = Mod,
                                        function = Fun,
                                        arguments = NewArgs,
                                        eventid = make_ref()
                                    },
                                    nsime_simulator:schedule_now(Event),
                                    {reply, true, RoutingState}
                            end
                    end;
                _ ->
                    case LocalDeliverCallback of
                        undefined ->
                            {reply, false, RoutingState};
                        _ ->
                            {Mod, Fun, Args} = LocalDeliverCallback,
                            NewArgs = lists:flatten([Args, [Packet, Ipv4Header, InterfacePid]]),
                            Event = #nsime_event{
                                module = Mod,
                                function = Fun,
                                arguments = NewArgs,
                                eventid = make_ref()
                            },
                            nsime_simulator:schedule_now(Event),
                            {reply, true, RoutingState}
                    end

            end
    end;

handle_call({route_output, _Packet, Ipv4Header, OutputNetdevice}, _From, RoutingState) ->
    DestinationAddress = nsime_ipv4_header:get_destination_address(Ipv4Header),
    Route = lookup_static(DestinationAddress, OutputNetdevice, RoutingState),
    case Route of
        undefined ->
            {reply, {error_noroutetohost, undefined}, RoutingState};
        _ ->
            {reply, {error_noterror, Route}, RoutingState}
    end;

handle_call({notify_interface_up, InterfacePid}, _From, RoutingState) ->
    {reply, ok, do_notify_interface_up(InterfacePid, RoutingState)};

handle_call({notify_interface_down, InterfacePid}, _From, RoutingState) ->
    {reply, ok, do_notify_interface_down(InterfacePid, RoutingState)};

handle_call({notify_add_address, InterfacePid, InterfaceAddress}, _From, RoutingState) ->
    case nsime_ipv4_interface:is_up(InterfacePid) of
        false ->
            {reply, ok, RoutingState};
        true ->
            case
                (nsime_ipv4_interface_address:get_local_address(InterfaceAddress)
                    =/= undefined) and
                (nsime_ipv4_interface_address:get_mask(InterfaceAddress) =/= undefined)
            of
                false ->
                    {reply, ok, RoutingState};
                true ->
                    NetworkRoutes =
                        RoutingState#nsime_ipv4_static_routing_state.network_routes,
                    Route = nsime_ipv4_routing_table_entry:create_network_route(
                          nsime_ipv4_address:combine_mask(
                              nsime_ipv4_interface_address:get_local_address(
                                  InterfaceAddress
                              ),
                              nsime_ipv4_interface_address:get_mask(InterfaceAddress)
                          ),
                          nsime_ipv4_interface_address:get_mask(InterfaceAddress),
                          InterfacePid,
                          0
                    ),
                    NewNetworkRoutes = [Route | NetworkRoutes],
                    NewRoutingState = RoutingState#nsime_ipv4_static_routing_state{
                        network_routes = NewNetworkRoutes
                    },
                    {reply, ok, NewRoutingState}
            end
    end;

handle_call(
    {
        notify_remove_address,
        InterfacePid,
        InterfaceAddress
    },
    _From,
    RoutingState
) ->
    case nsime_ipv4_interface:is_up(InterfacePid) of
        false ->
            {reply, ok, RoutingState};
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
                        (R#nsime_ipv4_routing_table_entry.interface == InterfacePid) and
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
            NewRoutingState = RoutingState#nsime_ipv4_static_routing_state{
                network_routes = NewNetworkRoutes
            },
            {reply, ok, NewRoutingState}
    end;

handle_call({set_ipv4_protocol, Ipv4ProtocolPid, InterfaceList}, _From, RoutingState) ->
    NewRoutingState =
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
    ),
    NewerRoutingState = NewRoutingState#nsime_ipv4_static_routing_state{
        ipv4_protocol = Ipv4ProtocolPid
    },
    {reply, ok, NewerRoutingState};

handle_call(
    {
        add_network_route,
        NetworkAddress,
        NetworkMask,
        InterfacePid,
        Metric
    },
    _From,
    RoutingState
) ->
    NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
    Route = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        NetworkMask,
        InterfacePid,
        Metric
    ),
    NewNetworkRoutes = [Route | NetworkRoutes],
    NewRoutingState = RoutingState#nsime_ipv4_static_routing_state{
        network_routes = NewNetworkRoutes
    },
    {reply, ok, NewRoutingState};

handle_call(
    {
        add_network_route,
        NetworkAddress,
        NetworkMask,
        NextHopAddress,
        InterfacePid,
        Metric
    },
    _From,
    RoutingState
) ->
    NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
    Route = nsime_ipv4_routing_table_entry:create_network_route(
        NetworkAddress,
        NetworkMask,
        NextHopAddress,
        InterfacePid,
        Metric
    ),
    NewNetworkRoutes = [Route | NetworkRoutes],
    NewRoutingState = RoutingState#nsime_ipv4_static_routing_state{
        network_routes = NewNetworkRoutes
    },
    {reply, ok, NewRoutingState};

handle_call(get_network_routes, _From, RoutingState) ->
    NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
    {reply, NetworkRoutes, RoutingState};

handle_call(terminate, _From, RoutingState) ->
    {stop, normal, stopped, RoutingState}.

handle_cast(_Request, RoutingState) ->
    {noreply, RoutingState}.

handle_info(_Request, RoutingState) ->
    {noreply, RoutingState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, RoutingState, _Extra) ->
    {ok, RoutingState}.

%% Helper methods %%

lookup_static(DestinationAddress, OutputNetdevice, RoutingState) ->
    case nsime_ipv4_address:is_local_multicast(DestinationAddress) of
        true ->
            InterfacePid = nsime_netdevice:get_interface(OutputNetdevice),
            [FirstAddressPid | _] = nsime_ipv4_interface:get_address_list(InterfacePid),
            SrcAddress = nsime_ipv4_interface_address:get_local_address(FirstAddressPid),
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
                            case
                            is_pid(OutputNetdevice) and
                            (
                                OutputNetdevice =/=
                                    nsime_ipv4_interface:get_device(
                                        R#nsime_ipv4_routing_table_entry.interface
                                    )
                            )
                            of
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
                                                    InterfacePid = R#nsime_ipv4_routing_table_entry.interface,
                                                    SrcAddress = source_address_selection(
                                                        InterfacePid,
                                                        CandidateAddress
                                                    ),
                                                    NewBestRoute = #nsime_ipv4_route{
                                                        destination = CandidateAddress,
                                                        source = SrcAddress,
                                                        gateway = R#nsime_ipv4_routing_table_entry.gateway,
                                                        output_device = nsime_ipv4_interface:get_device(
                                                            R#nsime_ipv4_routing_table_entry.interface
                                                        )
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

source_address_selection(InterfacePid, Address) ->
    case nsime_ipv4_interface:get_address_list(InterfacePid) of
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

do_notify_interface_up(InterfacePid, RoutingState) ->
    AddressList = nsime_ipv4_interface:get_address_list(InterfacePid),
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
                        InterfacePid,
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

do_notify_interface_down(InterfacePid, RoutingState) ->
    NetworkRoutes = RoutingState#nsime_ipv4_static_routing_state.network_routes,
    NewNetworkRoutes = lists:filter(
        fun(R) ->
            case R#nsime_ipv4_routing_table_entry.interface == InterfacePid of
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
