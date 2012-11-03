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

%% Purpose : IPv4 list routing protocol module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_list_routing).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_ipv4_routing_table_entry.hrl").
-include("nsime_ipv4_static_routing_state.hrl").
-include("nsime_ipv4_list_routing_state.hrl").

-export([create/0, route_input/10, route_input/11, route_output/4,
         notify_interface_up/3, notify_interface_down/3, notify_add_address/4,
         notify_remove_address/4, populate_network_routes/2, add_routing_protocol/3]).

create() ->
    {nsime_ipv4_list_routing, #nsime_ipv4_list_routing_state{}}.

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
    WeakEsModel
) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    DestinationAddress = nsime_ipv4_header:get_destination_address(Ipv4Header),
    [Interface] = lists:filter(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    case
        is_destination_address(
            DestinationAddress,
            Interface,
            InterfaceList,
            WeakEsModel
        )
    of
        true ->
            {Mod, Fun, Args} = LocalDeliverCallback,
            NewArgs = lists:flatten([Args, [Packet, Ipv4Header, Interface]]),
            Event = #nsime_event{
                module = Mod,
                function = Fun,
                arguments = NewArgs,
                eventid = make_ref()
            },
            nsime_simulator:schedule_now(Event),
            case nsime_ipv4_address:is_multicast(DestinationAddress) of
                false ->
                    true;
                true ->
                    case nsime_ipv4_interface:is_forwarding(Interface) of
                        false ->
                            {Mod1, Fun1, Args1} = ErrorCallback,
                            NewArgs1 = lists:flatten([Args1, [Packet, Ipv4Header, error_noroutetohost]]),
                            erlang:apply(Mod1, Fun1, NewArgs1),
                            false;
                        true ->
                            lists:foldl(
                                fun({_Priority, {ProtocolModule, ProtocolState}}, Success) ->
                                    case Success of
                                        true ->
                                            true;
                                        _ ->
                                            catch ProtocolModule:route_input(
                                                ProtocolState,
                                                Packet,
                                                Ipv4Header,
                                                IngressNetdevice,
                                                InterfaceId,
                                                UnicastForwardCallback,
                                                MulticastForwardCallback,
                                                undefined,
                                                ErrorCallback,
                                                InterfaceList,
                                                WeakEsModel
                                            )
                                    end
                                end,
                                false,
                                RoutingProtocols
                            )
                    end
            end;
        false ->
            case nsime_ipv4_interface:is_forwarding(Interface) of
                false ->
                    {Mod, Fun, Args} = ErrorCallback,
                    NewArgs = lists:flatten([Args, [Packet, Ipv4Header, error_noroutetohost]]),
                    erlang:apply(Mod, Fun, NewArgs),
                    false;
                true ->
                    lists:foldl(
                        fun({_Priority, {ProtocolModule, ProtocolState}}, Success) ->
                            case Success of
                                true ->
                                    true;
                                _ ->
                                    catch ProtocolModule:route_input(
                                        ProtocolState,
                                        Packet,
                                        Ipv4Header,
                                        IngressNetdevice,
                                        InterfaceId,
                                        UnicastForwardCallback,
                                        MulticastForwardCallback,
                                        LocalDeliverCallback,
                                        ErrorCallback,
                                        InterfaceList,
                                        WeakEsModel
                                    )
                            end
                        end,
                        false,
                        RoutingProtocols
                    )
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
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    Route = lists:foldl(
        fun({_Priority, {ProtocolModule, ProtocolState}}, CurrentRoute) ->
            case CurrentRoute of
                undefined ->
                    case ProtocolModule:route_output(
                        ProtocolState,
                        Ipv4Header,
                        OutputNetdevice,
                        InterfaceList
                    )
                    of
                        {error_noroutetohost, undefined} ->
                            undefined;
                        {error_noterror, NewRoute} ->
                            NewRoute
                    end;
                _ ->
                    CurrentRoute
            end
        end,
        undefined,
        RoutingProtocols
    ),
    case Route of
        undefined ->
            {error_noroutetohost, undefined};
        _ ->
            {error_noterror, Route}
    end.

notify_interface_up(RoutingState, InterfaceId, InterfaceList) ->
    NewRoutingProtocols =
    lists:map(
        fun({Priority, {ProtocolModule, ProtocolState}}) ->
            {Priority, 
                {
                    ProtocolModule, 
                    ProtocolModule:notify_interface_up(
                        ProtocolState,
                        InterfaceId,
                        InterfaceList
                    )
                }
            }
        end,
        RoutingState#nsime_ipv4_list_routing_state.routing_protocols
    ),
    RoutingState#nsime_ipv4_list_routing_state{
        routing_protocols = NewRoutingProtocols
    }.

notify_interface_down(RoutingState, InterfaceId, InterfaceList) ->
    NewRoutingProtocols =
    lists:map(
        fun({Priority, {ProtocolModule, ProtocolState}}) ->
            {Priority, 
                {
                    ProtocolModule, 
                    ProtocolModule:notify_interface_down(
                        ProtocolState,
                        InterfaceId,
                        InterfaceList
                    )
                }
            }
        end,
        RoutingState#nsime_ipv4_list_routing_state.routing_protocols
    ),
    RoutingState#nsime_ipv4_list_routing_state{
        routing_protocols = NewRoutingProtocols
    }.

notify_add_address(RoutingState, InterfaceId, InterfaceAddress, InterfaceList) ->
    NewRoutingProtocols =
    lists:map(
        fun({Priority, {ProtocolModule, ProtocolState}}) ->
            {Priority, 
                {
                    ProtocolModule, 
                    ProtocolModule:notify_add_address(
                        ProtocolState,
                        InterfaceId,
                        InterfaceAddress,
                        InterfaceList
                    )
                }
            }
        end,
        RoutingState#nsime_ipv4_list_routing_state.routing_protocols
    ),
    RoutingState#nsime_ipv4_list_routing_state{
        routing_protocols = NewRoutingProtocols
    }.

notify_remove_address(RoutingState, InterfaceId, InterfaceAddress, InterfaceList) ->
    NewRoutingProtocols =
    lists:map(
        fun({Priority, {ProtocolModule, ProtocolState}}) ->
            {Priority, 
                {
                    ProtocolModule, 
                    ProtocolModule:notify_remove_address(
                        ProtocolState,
                        InterfaceId,
                        InterfaceAddress,
                        InterfaceList
                    )
                }
            }
        end,
        RoutingState#nsime_ipv4_list_routing_state.routing_protocols
    ),
    RoutingState#nsime_ipv4_list_routing_state{
        routing_protocols = NewRoutingProtocols
    }.

populate_network_routes(RoutingState, InterfaceList) ->
    NewRoutingProtocols =
    lists:map(
        fun({Priority, {ProtocolModule, ProtocolState}}) ->
            {Priority, 
                {
                    ProtocolModule, 
                    ProtocolModule:populate_network_routes(
                        ProtocolState,
                        InterfaceList
                    )
                }
            }
        end,
        RoutingState#nsime_ipv4_list_routing_state.routing_protocols
    ),
    RoutingState#nsime_ipv4_list_routing_state{
        routing_protocols = NewRoutingProtocols
    }.

add_routing_protocol(RoutingState, {ProtocolModule, ProtocolState}, Priority) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    NewRoutingProtocols = lists:sort(
        fun({A, _}, {B, _}) ->
            A =< B
        end,
        [{Priority, {ProtocolModule, ProtocolState}} | RoutingProtocols]
    ),
    RoutingState#nsime_ipv4_list_routing_state{
        routing_protocols = NewRoutingProtocols
    }.

%% Helper methods %%

is_destination_address(Ipv4Address, Interface, InterfaceList, WeakEsModel) ->
    InterfaceAddressList = nsime_ipv4_interface:get_address_list(Interface),
    IsMatch = lists:foldl(
        fun(A, Result) ->
            case Result of
                true ->
                    true;
                false ->
                    (nsime_ipv4_interface_address:get_local_address(A) == Ipv4Address)
                    or
                    (nsime_ipv4_interface_address:get_broadcast_address(A) == Ipv4Address)
            end
        end,
        false,
        InterfaceAddressList
    ),
    case IsMatch of
        true ->
            true;
        false ->
            case
                nsime_ipv4_address:is_multicast(Ipv4Address) or
                nsime_ipv4_address:is_broadcast(Ipv4Address)
            of
                true ->
                    true;
                false ->
                    case WeakEsModel of
                        false ->
                            false;
                        true ->
                            OtherInterfaces = lists:filter(
                                fun(I) ->
                                    I =/= Interface
                                end,
                                InterfaceList
                            ),
                            IsMatch2 = lists:foldl(
                                fun(I, Match) ->
                                    case Match of
                                        true ->
                                            true;
                                        false ->
                                            InterAddrList = nsime_ipv4_interface:get_address_list(I),
                                            lists:foldl(
                                                fun(A, Result) ->
                                                    case Result of
                                                        true ->
                                                            true;
                                                        false ->
                                                            (nsime_ipv4_interface_address:get_local_address(A) == Ipv4Address)
                                                            or
                                                            (nsime_ipv4_interface_address:get_broadcast_address(A) == Ipv4Address)
                                                    end
                                                end,
                                                false,
                                                InterAddrList
                                            )
                                    end
                                end,
                                false,
                                OtherInterfaces
                            ),
                            IsMatch2
                    end
            end
    end.
