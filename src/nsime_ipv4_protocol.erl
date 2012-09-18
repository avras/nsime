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

%% Purpose : IPv4 protocol module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_protocol).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_ipv4_protocol_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, set_node/2,
         set_routing_protocol/2, get_routing_protocol/1,
         create_raw_socket/1, delete_raw_socket/2,
         insert_layer4_protocol/2, get_layer4_protocol/2,
         remove_layer4_protocol/2, set_default_ttl/2,
         recv/7, send/6, send_with_header/4,
         add_interface/2, get_interface_list/1,
         get_interface_for_address/2, get_interface_for_prefix/3,
         get_interface_for_device/2, is_destination_address/3,
         add_interface_address/3, remove_interface_address/3,
         get_interface_address_list/2, select_source_address/4,
         set_metric/3, get_metric/2,
         get_mtu/2, is_up/2, set_up/2, set_down/2, is_forwarding/2,
         set_forwarding/3, get_netdevice/2,
         set_fragment_expiration_timeout/2, set_transmit_trace/2,
         set_receive_trace/2, set_drop_trace/2,
         set_send_outgoing_trace/2, set_unicast_forward_trace/2,
         set_local_deliver_trace/2]).

-define(IPv4_PROTOCOL_NUMBER, 16#0800).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(ProtocolPid) ->
    gen_server:call(ProtocolPid, terminate).

set_node(ProtocolPid, NodePid) ->
    gen_server:call(ProtocolPid, {set_node, NodePid}).

set_routing_protocol(ProtocolPid, RoutingPid) ->
    gen_server:call(ProtocolPid, {set_routing_protocol, RoutingPid}).

get_routing_protocol(ProtocolPid) ->
    gen_server:call(ProtocolPid, get_routing_protocol).

create_raw_socket(_ProtocolPid) ->
    ok.

delete_raw_socket(_ProtocolPid, _SocketPid) ->
    ok.

insert_layer4_protocol(ProtocolPid, Layer4ProtocolPid) ->
    gen_server:call(ProtocolPid, {insert_layer4_protocol, Layer4ProtocolPid}).

get_layer4_protocol(ProtocolPid, Layer4ProtocolNumber) ->
    gen_server:call(ProtocolPid, {get_layer4_protocol, Layer4ProtocolNumber}).

remove_layer4_protocol(ProtocolPid, Layer4ProtocolPid) ->
    gen_server:call(ProtocolPid, {remove_layer4_protocol, Layer4ProtocolPid}).

set_default_ttl(ProtocolPid, DefaultTtl) ->
    gen_server:call(ProtocolPid, {set_default_ttl, DefaultTtl}).

add_interface(ProtocolPid, DevicePid) ->
    gen_server:call(ProtocolPid, {add_interface, DevicePid}).

get_interface_list(ProtocolPid) ->
    gen_server:call(ProtocolPid, get_interface_list).

get_interface_for_address(ProtocolPid, Address) ->
    gen_server:call(ProtocolPid, {get_interface_for_address, Address}).

get_interface_for_prefix(ProtocolPid, Address, Mask) ->
    gen_server:call(ProtocolPid, {get_interface_for_prefix, Address, Mask}).

get_interface_for_device(ProtocolPid, DevicePid) ->
    gen_server:call(ProtocolPid, {get_interface_for_device, DevicePid}).

is_destination_address(ProtocolPid, Ipv4Address, InterfacePid) ->
    gen_server:call(ProtocolPid, {is_destination_address, Ipv4Address, InterfacePid}).

add_interface_address(ProtocolPid, InterfacePid, InterfaceAddressPid) ->
    gen_server:call(ProtocolPid, {add_interface_address, InterfacePid, InterfaceAddressPid}).

remove_interface_address(ProtocolPid, InterfacePid, InterfaceAddressPid) ->
    gen_server:call(ProtocolPid, {remove_interface_address, InterfacePid, InterfaceAddressPid}).

get_interface_address_list(ProtocolPid, InterfacePid) ->
    gen_server:call(ProtocolPid, {get_interface_address_list, InterfacePid}).

select_source_address(ProtocolPid, DevicePid, DestAddress, InterfaceAddressScope) ->
    gen_server:call(ProtocolPid, {select_source_address, DevicePid, DestAddress, InterfaceAddressScope}).

set_metric(ProtocolPid, InterfacePid, Metric) ->
    gen_server:call(ProtocolPid, {set_metric, InterfacePid, Metric}).

get_metric(ProtocolPid, InterfacePid) ->
    gen_server:call(ProtocolPid, {get_metric, InterfacePid}).

get_mtu(ProtocolPid, InterfacePid) ->
    gen_server:call(ProtocolPid, {get_mtu, InterfacePid}).

is_up(ProtocolPid, InterfacePid) ->
    gen_server:call(ProtocolPid, {is_up, InterfacePid}).

set_up(ProtocolPid, InterfacePid) ->
    gen_server:call(ProtocolPid, {set_up, InterfacePid}).

set_down(ProtocolPid, InterfacePid) ->
    gen_server:call(ProtocolPid, {set_down, InterfacePid}).

is_forwarding(ProtocolPid, InterfacePid) ->
    gen_server:call(ProtocolPid, {is_forwarding, InterfacePid}).

set_forwarding(ProtocolPid, Forwarding) ->
    gen_server:call(ProtocolPid, {set_forwarding, Forwarding}).

get_netdevice(ProtocolPid, InterfacePid) ->
    gen_server:call(ProtocolPid, {get_netdevice, InterfacePid}).

set_fragment_expiration_timeout(ProtocolPid, Timeout) ->
    gen_server:call(ProtocolPid, {set_fragment_expiration_timeout, Timeout}).

set_transmit_trace(ProtocolPid, Callback) ->
    gen_server:call(ProtocolPid, {set_transmit_trace, Callback}).

set_receive_trace(ProtocolPid, Callback) ->
    gen_server:call(ProtocolPid, {set_receive_trace, Callback}).

set_drop_trace(ProtocolPid, Callback) ->
    gen_server:call(ProtocolPid, {set_drop_trace, Callback}).

set_send_outgoing_trace(ProtocolPid, TraceCallback) ->
    gen_server:call(ProtocolPid, {set_send_outgoing_trace, TraceCallback}).

set_unicast_forward_trace(ProtocolPid, TraceCallback) ->
    gen_server:call(ProtocolPid, {set_unicast_forward_trace, TraceCallback}).

set_local_deliver_trace(ProtocolPid, TraceCallback) ->
    gen_server:call(ProtocolPid, {set_local_deliver_trace, TraceCallback}).

init([]) ->
    ProtocolState = #nsime_ipv4_protocol_state{
        ipv4_endpoints_demux = nsime_ip_endpoint_demux:create()
    },
    {ok, ProtocolState}.

handle_call({set_node, NodePid}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{node = NodePid},
    {reply, ok, NewProtocolState};

handle_call({set_routing_protocol, RoutingPid}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        routing_protocol = RoutingPid
    },
    {reply, ok, NewProtocolState};

handle_call(get_routing_protocol, _From, ProtocolState) ->
    {reply, ProtocolState#nsime_ipv4_protocol_state.routing_protocol, ProtocolState};

handle_call({insert_layer4_protocol, Layer4ProtocolPid}, _From, ProtocolState) ->
    Layer4ProtocolList = ProtocolState#nsime_ipv4_protocol_state.layer4_protocols,
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        layer4_protocols = [Layer4ProtocolPid | Layer4ProtocolList]
    },
    {reply, ok, NewProtocolState};

handle_call({get_layer4_protocol, Layer4ProtocolNumber}, _From, ProtocolState) ->
    Layer4ProtocolList = ProtocolState#nsime_ipv4_protocol_state.layer4_protocols,
    Layer4ProtocolPid = lists:foldl(
        fun(P, Match) ->
            case is_pid(Match) of
            true ->
                Match;
            false ->
                case nsime_layer4_protocol:protocol_number(P) == Layer4ProtocolNumber of
                    true ->
                        P;
                    false ->
                        none
                end
            end
        end,
        none,
        Layer4ProtocolList
    ),
    {reply, Layer4ProtocolPid, ProtocolState};

handle_call({remove_layer4_protocol, Layer4ProtocolPid}, _From, ProtocolState) ->
    Layer4ProtocolList = ProtocolState#nsime_ipv4_protocol_state.layer4_protocols,
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        layer4_protocols = lists:delete(Layer4ProtocolPid, Layer4ProtocolList)
    },
    {reply, ok, NewProtocolState};

handle_call({set_default_ttl, DefaultTtl}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{default_ttl = DefaultTtl},
    {reply, ok, NewProtocolState};

handle_call({add_interface, DevicePid}, _From, ProtocolState) ->
    NodePid =  ProtocolState#nsime_ipv4_protocol_state.node,
    nsime_node:register_protocol_handler(
        NodePid,
        {
            ?MODULE,
            recv,
            [self()]
        },
        ?IPv4_PROTOCOL_NUMBER,
        DevicePid
    ),
    InterfacePid = nsime_ipv4_interface:create(),
    nsime_ipv4_interface:set_node(InterfacePid, NodePid),
    nsime_ipv4_interface:set_device(InterfacePid, DevicePid),
    nsime_ipv4_interface:set_forwarding(
        InterfacePid,
        ProtocolState#nsime_ipv4_protocol_state.ip_forward
    ),
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        interfaces = [InterfacePid | InterfaceList]
    },
    {reply, InterfacePid, NewProtocolState};

handle_call(get_interface_list, _From, ProtocolState) ->
    {reply, ProtocolState#nsime_ipv4_protocol_state.interfaces, ProtocolState};

handle_call({get_interface_for_address, Address}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    InterfacePid = lists:foldl(
        fun(I, Match) ->
            case is_pid(Match) of
                true ->
                    Match;
                false ->
                    InterfaceAddressList = nsime_ipv4_interface:get_address_list(I),
                    case lists:any(
                        fun(A) ->
                            nsime_ipv4_interface_address:get_local_address(A) ==
                                Address
                        end,
                        InterfaceAddressList
                    )
                    of
                        true ->
                            I;
                        false ->
                            none
                    end
            end
        end,
        none,
        InterfaceList
    ),
    {reply, InterfacePid, ProtocolState};

handle_call({get_interface_for_prefix, Address, Mask}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    InterfacePid = lists:foldl(
        fun(I, Match) ->
            case is_pid(Match) of
                true ->
                    Match;
                false ->
                    InterfaceAddressList = nsime_ipv4_interface:get_address_list(I),
                    case lists:any(
                        fun(A) ->
                            nsime_ipv4_address:combine_mask(
                                nsime_ipv4_interface_address:get_local_address(A),
                                Mask
                            ) == nsime_ipv4_address:combine_mask(Address, Mask)
                        end,
                        InterfaceAddressList
                    )
                    of
                        true ->
                            I;
                        false ->
                            none
                    end
            end
        end,
        none,
        InterfaceList
    ),
    {reply, InterfacePid, ProtocolState};

handle_call({get_interface_for_device, DevicePid}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    InterfacePid = lists:foldl(
        fun(I, Match) ->
            case is_pid(Match) of
                true ->
                    Match;
                false ->
                    case nsime_ipv4_interface:get_device(I) == DevicePid of
                        true ->
                            I;
                        false ->
                            none
                    end
            end
        end,
        none,
        InterfaceList
    ),
    {reply, InterfacePid, ProtocolState};

handle_call({is_destination_address, Ipv4Address, InterfacePid}, _From, ProtocolState) ->
    InterfaceAddressList = nsime_ipv4_interface:get_address_list(InterfacePid),
    IsMatch = lists:foldl(
        fun(A, Result) ->
            case Result of
                true ->
                    true;
                false ->
                    (nsime_ipv4_interface_address:get_local_address(A) == Ipv4Address)
                    bor
                    (nsime_ipv4_interface_address:get_broadcast_address(A) == Ipv4Address)
            end
        end,
        false,
        InterfaceAddressList
    ),
    case IsMatch of
        true ->
            {reply, true, ProtocolState};
        false ->
            case
                nsime_ipv4_address:is_multicast(Ipv4Address) bor
                nsime_ipv4_address:is_broadcast(Ipv4Address)
            of
                true ->
                    {reply, true, ProtocolState};
                false ->
                    case ProtocolState#nsime_ipv4_protocol_state.weak_es_model of
                        false ->
                            {reply, false, ProtocolState};
                        true ->
                            InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
                            OtherInterfaces = lists:filter(
                                fun(I) ->
                                    I =/= InterfacePid
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
                                                            bor
                                                            (nsime_ipv4_interface_address:get_broadcast_address(A) == Ipv4Address)
                                                    end
                                                end
                                                false,
                                                InterAddrList
                                            )
                                    end
                                end,
                                false,
                                OtherInterfaces
                            ),
                            {reply, IsMatch2, ProtocolState}
                    end
            end
    end;

handle_call({add_interface_address, InterfacePid, InterfaceAddressPid}, _From, ProtocolState) ->
    nsime_ipv4_interface:add_address(InterfacePid, InterfaceAddressPid),
    {reply, ok, ProtocolState};

handle_call({remove_interface_address, InterfacePid, InterfaceAddressPid}, _From, ProtocolState) ->
    nsime_ipv4_interface:remove_address(InterfacePid, InterfaceAddressPid),
    {reply, ok, ProtocolState};

handle_call({get_interface_address_list, InterfacePid}, _From, ProtocolState) ->
    {reply, nsime_ipv4_interface:get_address_list(InterfacePid), ProtocolState};

handle_call({select_source_address, DevicePid, DestAddress, InterfaceAddressScope}, _From, ProtocolState) ->
    {reply, ok, ProtocolState};

handle_call(terminate, _From, ProtocolState) ->
    lists:foreach(
        fun(S) ->
            nsime_udp_socket:destroy(S)
        end,
        Sockets
    ),
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
    },
    {stop, normal, stopped, NewProtocolState}.

handle_cast(_Request, ProtocolState) ->
    {noreply, ProtocolState}.

handle_info(_Request, ProtocolState) ->
    {noreply, ProtocolState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, ProtocolState, _Extra) ->
    {ok, ProtocolState}.
