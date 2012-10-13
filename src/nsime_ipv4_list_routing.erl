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

-include("nsime_ipv4_list_routing_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, route_input/9, route_output/4,
         notify_interface_up/2, notify_interface_down/2, notify_add_address/3,
         notify_remove_address/3, set_ipv4_protocol/3, add_routing_protocol/3]).

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
    UnicastForwardCallback,
    MulticastForwardCallback,
    LocalDeliverCallback,
    ErrorCallback,
    InterfaceList
) ->
    case
        gen_server:call(RoutingPid, {route_input,
                                     Packet,
                                     Ipv4Header,
                                     IngressNetdevice,
                                     UnicastForwardCallback,
                                     MulticastForwardCallback,
                                     LocalDeliverCallback,
                                     ErrorCallback,
                                     InterfaceList
                                    })
    of
        options_not_supported ->
            options_not_supported;
        true ->
            true;
        false ->
            false
    end.

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

add_routing_protocol(RoutingPid, ProtocolPid, Priority) ->
    gen_server:call(RoutingPid, {add_routing_protocol, ProtocolPid, Priority}).

init([]) ->
    RoutingState = #nsime_ipv4_list_routing_state{},
    {ok, RoutingState}.

handle_call(
    {
        route_input,
        Packet,
        Ipv4Header,
        IngressNetdevice,
        UnicastForwardCallback,
        MulticastForwardCallback,
        LocalDeliverCallback,
        ErrorCallback,
        InterfaceList
    },
    _From,
    RoutingState
) ->
    Ipv4ProtocolPid = RoutingState#nsime_ipv4_list_routing_state.ipv4_protocol,
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    InterfacePid = nsime_netdevice:get_interface(IngressNetdevice),
    DestinationAddress = nsime_ipv4_header:get_destination_address(Ipv4Header),
    case
        nsime_ipv4_protocol:is_destination_address(
            Ipv4ProtocolPid,
            DestinationAddress,
            InterfacePid
        )
    of
        true ->
            {Mod, Fun, Args} = LocalDeliverCallback,
            NewArgs = lists:flatten([Args, [Packet, Ipv4Header, InterfacePid]]),
            erlang:apply(Mod, Fun, NewArgs),
            case nsime_ipv4_address:is_multicast(DestinationAddress) of
                false ->
                    {reply, true, RoutingState};
                true ->
                    case nsime_ipv4_interface:is_forwarding(InterfacePid) of
                        false ->
                            {Mod1, Fun1, Args1} = ErrorCallback,
                            NewArgs1 = lists:flatten([Args1, [Packet, Ipv4Header, error_noroutetohost]]),
                            erlang:apply(Mod1, Fun1, NewArgs1),
                            {reply, false, RoutingState};
                        true ->
                            ReturnValue =
                            lists:foldl(
                                fun({_Priority, ProtocolPid}, Success) ->
                                    case Success of
                                        true ->
                                            true;
                                        _ ->
                                            catch nsime_ipv4_routing_protocol:route_input(
                                                ProtocolPid,
                                                Packet,
                                                Ipv4Header,
                                                IngressNetdevice,
                                                UnicastForwardCallback,
                                                MulticastForwardCallback,
                                                undefined,
                                                ErrorCallback,
                                                InterfaceList
                                            )
                                    end
                                end,
                                false,
                                RoutingProtocols
                            ),
                            {reply, ReturnValue, RoutingState}
                    end
            end;
        false ->
            case nsime_ipv4_interface:is_forwarding(InterfacePid) of
                false ->
                    {Mod, Fun, Args} = ErrorCallback,
                    NewArgs = lists:flatten([Args, [Packet, Ipv4Header, error_noroutetohost]]),
                    erlang:apply(Mod, Fun, NewArgs),
                    {reply, false, RoutingState};
                true ->
                    ReturnValue =
                    lists:foldl(
                        fun({_Priority, ProtocolPid}, Success) ->
                            case Success of
                                true ->
                                    true;
                                _ ->
                                    catch nsime_ipv4_routing_protocol:route_input(
                                        ProtocolPid,
                                        Packet,
                                        Ipv4Header,
                                        IngressNetdevice,
                                        UnicastForwardCallback,
                                        MulticastForwardCallback,
                                        LocalDeliverCallback,
                                        ErrorCallback,
                                        InterfaceList
                                    )
                            end
                        end,
                        false,
                        RoutingProtocols
                    ),
                    {reply, ReturnValue, RoutingState}
            end
    end;

handle_call({route_output, Packet, Ipv4Header, OutputNetdevice}, _From, RoutingState) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    Route = lists:foldl(
        fun({_Priority, ProtocolPid}, CurrentRoute) ->
            case CurrentRoute of
                undefined ->
                    case nsime_ipv4_routing_protocol:route_output(
                        ProtocolPid,
                        Packet,
                        Ipv4Header,
                        OutputNetdevice
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
            {reply, {error_noroutetohost, undefined}, RoutingState};
        _ ->
            {reply, {error_noterror, Route}, RoutingState}
    end;

handle_call({notify_interface_up, InterfacePid}, _From, RoutingState) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    lists:foreach(
        fun({_Priority, ProtocolPid}) ->
            nsime_ipv4_routing_protocol:notify_interface_up(
                ProtocolPid,
                InterfacePid
            )
        end,
        RoutingProtocols
    ),
    {reply, ok, RoutingState};

handle_call({notify_interface_down, InterfacePid}, _From, RoutingState) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    lists:foreach(
        fun({_Priority, ProtocolPid}) ->
            nsime_ipv4_routing_protocol:notify_interface_down(
                ProtocolPid,
                InterfacePid
            )
        end,
        RoutingProtocols
    ),
    {reply, ok, RoutingState};

handle_call({notify_add_address, InterfacePid, InterfaceAddress}, _From, RoutingState) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    lists:foreach(
        fun({_Priority, ProtocolPid}) ->
            nsime_ipv4_routing_protocol:notify_add_address(
                ProtocolPid,
                InterfacePid,
                InterfaceAddress
            )
        end,
        RoutingProtocols
    ),
    {reply, ok, RoutingState};

handle_call({notify_remove_address, InterfacePid, InterfaceAddress}, _From, RoutingState) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    lists:foreach(
        fun({_Priority, ProtocolPid}) ->
            nsime_ipv4_routing_protocol:notify_remove_address(
                ProtocolPid,
                InterfacePid,
                InterfaceAddress
            )
        end,
        RoutingProtocols
    ),
    {reply, ok, RoutingState};

handle_call({set_ipv4_protocol, Ipv4ProtocolPid, InterfaceList}, _From, RoutingState) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    lists:foreach(
        fun({_Priority, ProtocolPid}) ->
            nsime_ipv4_routing_protocol:set_ipv4_protocol(
                ProtocolPid,
                Ipv4ProtocolPid,
                InterfaceList
            )
        end,
        RoutingProtocols
    ),
    NewRoutingState = RoutingState#nsime_ipv4_list_routing_state{
        ipv4_protocol = Ipv4ProtocolPid
    },
    {reply, ok, NewRoutingState};

handle_call({add_routing_protocol, ProtocolPid, Priority}, _From, RoutingState) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    NewRoutingProtocols = lists:sort(
        fun({A, _}, {B, _}) ->
            A =< B
        end,
        [{Priority, ProtocolPid} | RoutingProtocols]
    ),
    NewRoutingState = RoutingState#nsime_ipv4_list_routing_state{
        routing_protocols = NewRoutingProtocols
    },
    {reply, ok, NewRoutingState};

handle_call(terminate, _From, RoutingState) ->
    RoutingProtocols = RoutingState#nsime_ipv4_list_routing_state.routing_protocols,
    lists:foreach(
        fun({_Priority, R}) ->
            nsime_ipv4_routing_protocol:destroy(R)
        end,
        RoutingProtocols
    ),
    {stop, normal, stopped, RoutingState}.

handle_cast(_Request, RoutingState) ->
    {noreply, RoutingState}.

handle_info(_Request, RoutingState) ->
    {noreply, RoutingState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, RoutingState, _Extra) ->
    {ok, RoutingState}.
