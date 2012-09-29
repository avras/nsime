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
-include("nsime_event.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_ipv4_route.hrl").
-include("nsime_ipv4_protocol_state.hrl").
-include("nsime_icmpv4_protocol_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, set_node/2, protocol_number/0,
         set_routing_protocol/2, get_routing_protocol/1,
         create_raw_socket/1, delete_raw_socket/2,
         insert_layer4_protocol/2, get_layer4_protocol/2,
         remove_layer4_protocol/2, set_default_ttl/2,
         recv/6, send/6, send_with_header/4,
         add_interface/2, get_interface_list/1,
         get_interface_for_address/2, get_interface_for_prefix/3,
         get_interface_for_device/2, is_destination_address/3,
         add_interface_address/3, remove_interface_address/3,
         get_interface_address_list/2, select_source_address/4,
         set_metric/3, get_metric/2,
         get_mtu/2, is_up/2, set_up/2, set_down/2, is_forwarding/2,
         set_forwarding/3, get_netdevice/2,
         set_weak_es_model/2, get_weak_es_model/1,
         set_fragment_expiration_timeout/2, set_transmit_trace/2,
         set_receive_trace/2, set_drop_trace/2,
         set_send_outgoing_trace/2, set_unicast_forward_trace/2,
         set_local_deliver_trace/2, ip_forward/4, local_deliver/4,
         route_input_error/3]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(ProtocolPid) ->
    gen_server:call(ProtocolPid, terminate).

set_node(ProtocolPid, NodePid) ->
    gen_server:call(ProtocolPid, {set_node, NodePid}).

protocol_number() ->
    ?IPv4_PROTOCOL_NUMBER.

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

set_default_ttl(ProtocolPid, DefaulTTL) ->
    gen_server:call(ProtocolPid, {set_default_ttl, DefaulTTL}).

recv(ProtocolPid, DevicePid, Packet, Protocol, FromAddress, ToAddress) ->
    gen_server:call(ProtocolPid, {recv, DevicePid, Packet, Protocol, FromAddress, ToAddress}).

send(ProtocolPid, Packet, SrcAddress, DestAddress, Protocol, Route) ->
    gen_server:call(ProtocolPid, {send, Packet, SrcAddress, DestAddress, Protocol, Route}).

send_with_header(ProtocolPid, Packet, Ipv4Header, Route) ->
    gen_server:call(ProtocolPid, {send_with_header, Packet, Ipv4Header, Route}).

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

set_forwarding(ProtocolPid, InterfacePid, Forwarding) ->
    gen_server:call(ProtocolPid, {set_forwarding, InterfacePid, Forwarding}).

get_netdevice(ProtocolPid, InterfacePid) ->
    gen_server:call(ProtocolPid, {get_netdevice, InterfacePid}).

set_weak_es_model(ProtocolPid, WeakEsModel) ->
    gen_server:call(ProtocolPid, {set_weak_es_model, WeakEsModel}).

get_weak_es_model(ProtocolPid) ->
    gen_server:call(ProtocolPid, get_weak_es_model).

set_fragment_expiration_timeout(ProtocolPid, Timeout) ->
    gen_server:call(ProtocolPid, {set_fragment_expiration_timeout, Timeout}).

set_transmit_trace(ProtocolPid, TraceCallback) ->
    gen_server:call(ProtocolPid, {set_transmit_trace, TraceCallback}).

set_receive_trace(ProtocolPid, TraceCallback) ->
    gen_server:call(ProtocolPid, {set_receive_trace, TraceCallback}).

set_drop_trace(ProtocolPid, TraceCallback) ->
    gen_server:call(ProtocolPid, {set_drop_trace, TraceCallback}).

set_send_outgoing_trace(ProtocolPid, TraceCallback) ->
    gen_server:call(ProtocolPid, {set_send_outgoing_trace, TraceCallback}).

set_unicast_forward_trace(ProtocolPid, TraceCallback) ->
    gen_server:call(ProtocolPid, {set_unicast_forward_trace, TraceCallback}).

set_local_deliver_trace(ProtocolPid, TraceCallback) ->
    gen_server:call(ProtocolPid, {set_local_deliver_trace, TraceCallback}).

ip_forward(ProtocolPid, Route, Packet, Ipv4Header) ->
    gen_server:call(ProtocolPid, {ip_forward, Route, Packet, Ipv4Header}).

local_deliver(ProtocolPid, Packet, Ipv4Header, InterfacePid) ->
    gen_server:call(ProtocolPid, {local_deliver, Packet, Ipv4Header, InterfacePid}).

route_input_error(ProtocolPid, Packet, Ipv4Header) ->
    gen_server:call(ProtocolPid, {route_input_error, Packet, Ipv4Header}).

init([]) ->
    ProtocolState = #nsime_ipv4_protocol_state{},
    {ok, ProtocolState}.

handle_call({set_node, NodePid}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{node = NodePid},
    {reply, ok, NewProtocolState};

handle_call(protocol_number, _From, ProtocolState) ->
    {reply, ?IPv4_PROTOCOL_NUMBER, ProtocolState};

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

handle_call({set_default_ttl, DefaulTTL}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{default_ttl = DefaulTTL},
    {reply, ok, NewProtocolState};

handle_call({recv, DevicePid, Packet, _Protocol, _FromAddress, _ToAddress}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    InterfacePid = lists:foldl(
        fun(I, Found) ->
            case is_pid(Found) of
                true ->
                    Found;
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
    case nsime_ipv4_interface:is_up(InterfacePid) of
        false ->
            {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
            <<Ipv4HeaderBinary:160, Data/binary>> = Packet#nsime_packet.data,
            Ipv4Header = nsime_ipv4_header:deserialize(Ipv4HeaderBinary),
            NewPacket = Packet#nsime_packet{data = <<Data/binary>>},
            NewArgs = lists:flatten([Args | [Ipv4Header, NewPacket, drop_interface_down, self(), InterfacePid]]),
            erlang:apply(Mod, Fun, NewArgs),
            {reply, ok, ProtocolState};
        true ->
            {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.receive_trace,
            NewArgs = lists:flatten([Args | [Packet, self(), InterfacePid]]),
            erlang:apply(Mod, Fun, NewArgs),
            <<Ipv4HeaderBinary:160, Data/binary>> = Packet#nsime_packet.data,
            Ipv4Header = nsime_ipv4_header:deserialize(Ipv4HeaderBinary),
            NewIpv4Header = case nsime_config:checksum_enabled() of
                true ->
                    nsime_ipv4_header:enable_checksum(Ipv4Header);
                false ->
                    Ipv4Header
            end,
            NewPacket = Packet#nsime_packet{data = <<Data/binary>>},
            case nsime_ipv4_header:is_checksum_ok(NewIpv4Header) of
                false ->
                    {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                    NewArgs = lists:flatten([Args | [NewIpv4Header, NewPacket, drop_bad_checksum, self(), InterfacePid]]),
                    erlang:apply(Mod, Fun, NewArgs),
                    {reply, ok, ProtocolState};
                true ->
                    RoutingProtocolPid = ProtocolState#nsime_ipv4_protocol_state.routing_protocol,
                    case nsime_ipv4_routing_protocol:route_input(
                        RoutingProtocolPid,
                        NewPacket,
                        NewIpv4Header,
                        DevicePid,
                        {?MODULE, ip_forward, [self()]},
                        none,
                        {?MODULE, local_deliver, [self()]},
                        {?MODULE, route_input_error, [self()]}
                    ) of
                        false ->
                            {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                            NewArgs = lists:flatten([Args | [Ipv4Header, NewPacket, drop_no_route, self(), InterfacePid]]),
                            erlang:apply(Mod, Fun, NewArgs),
                            {reply, ok, ProtocolState};
                        true ->
                            {reply, ok, ProtocolState}
                    end
            end
    end;

handle_call({send, Packet, SrcAddress, DestAddress, Protocol, Route}, _From, ProtocolState) ->
    Tags = Packet#nsime_packet.tags,
    DefaulTTL = ProtocolState#nsime_ipv4_protocol_state.default_ttl,
    {TTL, NewTags} = case proplists:is_defined(socket_ip_ttl_tag, Tags) of
        true ->
            {proplists:get_value(socket_ip_ttl_tag, Tags), proplists:delete(socket_ip_ttl_tag, Tags)};
        false ->
            {DefaulTTL, Tags}
    end,
    NewPacket = Packet#nsime_packet{tags = NewTags},
    case nsime_ipv4_address:is_broadcast(DestAddress) or nsime_ipv4_address:is_local_multicast(DestAddress) of
        true ->
            {Ipv4Header, NewProtocolState} = build_header(
                SrcAddress,
                DestAddress,
                Protocol,
                Packet#nsime_packet.size,
                TTL,
                true,
                ProtocolState
            ),
            InterfaceList = NewProtocolState#nsime_ipv4_protocol_state.interfaces,
            lists:foreach(
                fun(I) ->
                    {Mod1, Fun1, Args1} = NewProtocolState#nsime_ipv4_protocol_state.send_outgoing_trace,
                    NewArgs1 = lists:flatten([Args1 | [Ipv4Header, NewPacket, I]]),
                    erlang:apply(Mod1, Fun1, NewArgs1),
                    Data = NewPacket#nsime_packet.data,
                    Ipv4HeaderBinary = nsime_ipv4_header:serialize(Ipv4Header),
                    NewerPacket = NewPacket#nsime_packet{
                        data = <<Ipv4HeaderBinary/binary, Data/binary>>,
                        size = Ipv4Header#nsime_ipv4_header.total_length
                    },
                    {Mod2, Fun2, Args2} = NewProtocolState#nsime_ipv4_protocol_state.transmit_trace,
                    NewArgs2 = lists:flatten([Args2 | [NewerPacket, self(), I]]),
                    erlang:apply(Mod2, Fun2, NewArgs2),
                    nsime_ipv4_interface:send(I, NewerPacket, DestAddress)
                end,
                InterfaceList
            ),
            {reply, ok, NewProtocolState};
        false ->
            InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
            FoundMatch = lists:foldl(
                fun(I, Match) ->
                    case Match of
                        true ->
                            true;
                        false ->
                            InterfaceAddressList = nsime_ipv4_interface:get_address_list(I),
                            lists:foldl(
                                fun(A, Match2) ->
                                    case Match2 of
                                        true ->
                                            true;
                                        false ->
                                            Mask = nsime_ipv4_interface_address:get_mask(A),
                                            LocalAddress = nsime_ipv4_interface_address:get_local_address(A),
                                            case
                                                nsime_ipv4_address:is_subnet_directed_broadcast(DestAddress, Mask) and
                                                (
                                                    nsime_ipv4_address:combine_mask(DestAddress, Mask) ==
                                                    nsime_ipv4_address:combine_mask(LocalAddress, Mask)
                                                )
                                            of
                                                true ->
                                                    {Ipv4Header, NewProtocolState} = build_header(
                                                        SrcAddress,
                                                        DestAddress,
                                                        Protocol,
                                                        Packet#nsime_packet.size,
                                                        TTL,
                                                        true,
                                                        ProtocolState
                                                    ),
                                                    {Mod1, Fun1, Args1} = NewProtocolState#nsime_ipv4_protocol_state.send_outgoing_trace,
                                                    NewArgs1 = lists:flatten([Args1 | [Ipv4Header, NewPacket, I]]),
                                                    erlang:apply(Mod1, Fun1, NewArgs1),
                                                    Data = NewPacket#nsime_packet.data,
                                                    Ipv4HeaderBinary = nsime_ipv4_header:serialize(Ipv4Header),
                                                    NewerPacket = NewPacket#nsime_packet{
                                                        data = <<Ipv4HeaderBinary/binary, Data/binary>>,
                                                        size = Ipv4Header#nsime_ipv4_header.total_length
                                                    },
                                                    {Mod2, Fun2, Args2} = NewProtocolState#nsime_ipv4_protocol_state.transmit_trace,
                                                    NewArgs2 = lists:flatten([Args2 | [NewerPacket, self(), I]]),
                                                    erlang:apply(Mod2, Fun2, NewArgs2),
                                                    nsime_ipv4_interface:send(I, NewerPacket, DestAddress),
                                                    true;
                                                false ->
                                                    false
                                            end
                                    end
                                end,
                                false,
                                InterfaceAddressList
                            )
                    end
                end,
                false,
                InterfaceList
            ),
            case FoundMatch of
                true ->
                    {reply, ok, ProtocolState};
                false ->
                    case is_record(Route, nsime_ipv4_route) of
                        true ->
                            case (nsime_ipv4_route:get_gateway(Route) =/= nsime_ipv4_address:get_zero()) of
                                true ->
                                    {Ipv4Header, NewProtocolState} = build_header(
                                        SrcAddress,
                                        DestAddress,
                                        Protocol,
                                        Packet#nsime_packet.size,
                                        TTL,
                                        true,
                                        ProtocolState
                                    ),
                                    DevicePid = Route#nsime_ipv4_route.output_device,
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
                                    {Mod1, Fun1, Args1} = NewProtocolState#nsime_ipv4_protocol_state.send_outgoing_trace,
                                    NewArgs1 = lists:flatten([Args1 | [Ipv4Header, NewPacket, InterfacePid]]),
                                    erlang:apply(Mod1, Fun1, NewArgs1),
                                    send_real_out(Route, NewPacket, Ipv4Header, NewProtocolState);
                                false ->
                                    erlang:error(nsime_situation_not_implemented)
                            end;
                        false ->
                            {Ipv4Header, NewProtocolState} = build_header(
                                SrcAddress,
                                DestAddress,
                                Protocol,
                                Packet#nsime_packet.size,
                                TTL,
                                true,
                                ProtocolState
                            ),
                            RoutingProtocolPid = ProtocolState#nsime_ipv4_protocol_state.routing_protocol,
                            case is_pid(RoutingProtocolPid) of
                                true ->
                                    case nsime_ipv4_routing_protocol:route_output(RoutingProtocolPid, NewPacket, Ipv4Header, node) of
                                        {error_noterror, NewRoute} ->
                                            DevicePid = NewRoute#nsime_ipv4_route.output_device,
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
                                            {Mod1, Fun1, Args1} = NewProtocolState#nsime_ipv4_protocol_state.send_outgoing_trace,
                                            NewArgs1 = lists:flatten([Args1 | [Ipv4Header, NewPacket, InterfacePid]]),
                                            erlang:apply(Mod1, Fun1, NewArgs1),
                                            send_real_out(NewRoute, NewPacket, Ipv4Header, NewProtocolState);
                                        {error_noroutetohost, undefined} ->
                                            {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                                            NewArgs = lists:flatten([Args | [Ipv4Header, NewPacket, drop_no_route, self(), none]]),
                                            erlang:apply(Mod, Fun, NewArgs),
                                            {reply, ok, NewProtocolState}
                                    end;
                                false ->
                                    erlang:error(nsime_routing_protocol_undefined)
                            end
                    end
            end
    end;

handle_call({send_with_header, Packet, Ipv4Header, Route}, _From, ProtocolState) ->
    send_real_out(Route, Packet, Ipv4Header, ProtocolState);

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
        DevicePid,
        false
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
                    or
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
                nsime_ipv4_address:is_multicast(Ipv4Address) or
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

handle_call({select_source_address, _DevicePid, _DestAddress, _InterfaceAddressScope}, _From, ProtocolState) ->
    {reply, ok, ProtocolState};

handle_call({set_metric, InterfacePid, Metric}, _From, ProtocolState) ->
    {reply, nsime_ipv4_interface:set_metric(InterfacePid, Metric), ProtocolState};

handle_call({get_metric, InterfacePid}, _From, ProtocolState) ->
    {reply, nsime_ipv4_interface:get_metric(InterfacePid), ProtocolState};

handle_call({get_mtu, InterfacePid}, _From, ProtocolState) ->
    Mtu = nsime_netdevice:get_mtu(nsime_ipv4_interface:get_device(InterfacePid)),
    {reply, Mtu, ProtocolState};

handle_call({is_up, InterfacePid}, _From, ProtocolState) ->
    {reply, nsime_ipv4_interface:is_up(InterfacePid), ProtocolState};

handle_call({set_up, InterfacePid}, _From, ProtocolState) ->
    nsime_ipv4_interface:set_up(InterfacePid),
    RoutingProtocolPid = ProtocolState#nsime_ipv4_protocol_state.routing_protocol,
    case is_pid(RoutingProtocolPid) of
        true ->
            nsime_ipv4_routing_protocol:notify_interface_up(
                RoutingProtocolPid,
                InterfacePid
            ),
            {reply, ok, ProtocolState};
        false ->
            {reply, ok, ProtocolState}
    end;

handle_call({set_down, InterfacePid}, _From, ProtocolState) ->
    nsime_ipv4_interface:set_down(InterfacePid),
    RoutingProtocolPid = ProtocolState#nsime_ipv4_protocol_state.routing_protocol,
    case is_pid(RoutingProtocolPid) of
        true ->
            nsime_ipv4_routing_protocol:notify_interface_down(
                RoutingProtocolPid,
                InterfacePid
            ),
            {reply, ok, ProtocolState};
        false ->
            {reply, ok, ProtocolState}
    end;

handle_call({is_forwarding, InterfacePid}, _From, ProtocolState) ->
    {reply, nsime_ipv4_interface:is_forwarding(InterfacePid), ProtocolState};

handle_call({set_forwarding, InterfacePid, Forwarding}, _From, ProtocolState) ->
    nsime_ipv4_interface:set_forwarding(InterfacePid, Forwarding),
    {reply, ok, ProtocolState};

handle_call({get_netdevice, InterfacePid}, _From, ProtocolState) ->
    {reply, nsime_ipv4_interface:get_device(InterfacePid), ProtocolState};

handle_call({set_weak_es_model, WeakEsModel}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        weak_es_model = WeakEsModel
    },
    {reply, ok, NewProtocolState};

handle_call(get_weak_es_model, _From, ProtocolState) ->
    {reply, ProtocolState#nsime_ipv4_protocol_state.weak_es_model, ProtocolState};

handle_call({set_fragment_expiration_timeout, Timeout}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        fragment_expiration_timeout = Timeout
    },
    {reply, ok, NewProtocolState};

handle_call({set_transmit_trace, TraceCallback}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        transmit_trace = TraceCallback
    },
    {reply, ok, NewProtocolState};

handle_call({set_receive_trace, TraceCallback}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        receive_trace = TraceCallback
    },
    {reply, ok, NewProtocolState};

handle_call({set_drop_trace, TraceCallback}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        drop_trace = TraceCallback
    },
    {reply, ok, NewProtocolState};

handle_call({set_send_outgoing_trace, TraceCallback}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        send_outgoing_trace = TraceCallback
    },
    {reply, ok, NewProtocolState};

handle_call({set_unicast_forward_trace, TraceCallback}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        unicast_forward_trace = TraceCallback
    },
    {reply, ok, NewProtocolState};

handle_call({set_local_deliver_trace, TraceCallback}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        local_deliver_trace = TraceCallback
    },
    {reply, ok, NewProtocolState};

handle_call({ip_forward, Route, Packet, Ipv4Header}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    DevicePid = Route#nsime_ipv4_route.output_device,
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
    NewIpv4Header = nsime_ipv4_header:set_ttl(nsime_ipv4_header:get_ttl(Ipv4Header) - 1),
    case nsime_ipv4_header:get_ttl() == 0 of
        true ->
            case
                (nsime_ipv4_header:get_protocol(NewIpv4Header) =/= nsime_icmpv4_protocol:protocol_number())
                and (nsime_ipv4_header:is_broadcast(NewIpv4Header) == false)
                and (nsime_ipv4_header:is_multicast(NewIpv4Header) == false)
            of
                true ->
                    ProtocolList = ProtocolState#nsime_ipv4_protocol_state.layer4_protocols,
                    [IcmpPid | _] = lists:filter(
                        fun(P) ->
                            nsime_layer4_protocol:protocol_number(P) == nsime_icmpv4_protocol:protocol_number()
                        end,
                        ProtocolList
                    ),
                    nsime_icmpv4_protocol:send_time_exceeded_ttl(IcmpPid, NewIpv4Header, Packet),
                    {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                    NewArgs = lists:flatten([Args | [NewIpv4Header, Packet, drop_ttl_expired, self(), InterfacePid]]),
                    erlang:apply(Mod, Fun, NewArgs),
                    {reply, ok, ProtocolState};
                false ->
                    {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                    NewArgs = lists:flatten([Args | [NewIpv4Header, Packet, drop_ttl_expired, self(), InterfacePid]]),
                    erlang:apply(Mod, Fun, NewArgs),
                    {reply, ok, ProtocolState}
            end;
        false ->
            {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.unicast_forward_trace,
            NewArgs = lists:flatten([Args | [NewIpv4Header, Packet, InterfacePid]]),
            erlang:apply(Mod, Fun, NewArgs),
            send_real_out(Route, Packet, NewIpv4Header, ProtocolState)
    end;

handle_call({local_deliver, Packet, Ipv4Header, InterfacePid}, _From, ProtocolState) ->
    {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.local_deliver_trace,
    NewArgs = lists:flatten([Args | [Ipv4Header, Packet, InterfacePid]]),
    erlang:apply(Mod, Fun, NewArgs),
    [Layer4ProtocolPid | _] = lists:filter(
        fun(P) ->
            nsime_layer4_protocol:protocol_number(P) ==
            nsime_ipv4_header:get_protocol(Ipv4Header)
        end,
        ProtocolState#nsime_ipv4_protocol_state.layer4_protocols
    ),
    case is_pid(Layer4ProtocolPid) of
        true ->
            RxStatus = nsime_layer4_protocol:recv(Layer4ProtocolPid, Packet, Ipv4Header, InterfacePid),
            case RxStatus of
                rx_ok ->
                    {reply, ok, ProtocolState};
                rx_endpoint_closed ->
                    {reply, ok, ProtocolState};
                rx_csum_failed ->
                    {reply, ok, ProtocolState};
                rx_endpoint_unreach ->
                    DestAddress = nsime_ipv4_header:get_destination_address(Ipv4Header),
                    case
                        (nsime_ipv4_address:is_broadcast(DestAddress)) or
                        (nsime_ipv4_address:is_multicast(DestAddress))
                    of
                        true ->
                            {reply, ok, ProtocolState};
                        false ->
                            InterfaceAddressList = nsime_ipv4_interface:get_address_list(InterfacePid),
                            SubnetDirected = lists:foldl(
                                fun(A, Directed) ->
                                    case Directed of
                                        true ->
                                            true;
                                        false ->
                                            Mask = nsime_ipv4_interface_address:get_mask(A),
                                            LocalAddress = nsime_ipv4_interface_address:get_local_address(A),
                                            nsime_ipv4_address:is_subnet_directed_broadcast(DestAddress, Mask) and
                                            (
                                                nsime_ipv4_address:combine_mask(DestAddress, Mask) ==
                                                nsime_ipv4_address:combine_mask(LocalAddress, Mask)
                                            )
                                    end
                                end,
                                false,
                                InterfaceAddressList
                            ),
                            case SubnetDirected of
                                false ->
                                    [IcmpPid | _] = lists:filter(
                                        fun(P) ->
                                            nsime_layer4_protocol:protocol_number(P) == nsime_icmpv4_protocol:protocol_number()
                                        end,
                                        ProtocolState#nsime_ipv4_protocol_state.layer4_protocols
                                    ),
                                    nsime_icmpv4_protocol:send_dest_unreach_port(IcmpPid, Ipv4Header, Packet),
                                    {reply, ok, ProtocolState};
                                true ->
                                    {reply, ok, ProtocolState}
                            end
                    end
            end;
        false ->
            {reply, ok, ProtocolState}
    end;

handle_call({route_input_error, Packet, Ipv4Header}, _From, ProtocolState) ->
    {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
    NewArgs = lists:flatten([Args | [Ipv4Header, Packet, drop_route_error, self(), none]]),
    erlang:apply(Mod, Fun, NewArgs),
    {reply, ok, ProtocolState};

handle_call(terminate, _From, ProtocolState) ->
    {stop, normal, stopped, ProtocolState}.

handle_cast(_Request, ProtocolState) ->
    {noreply, ProtocolState}.

handle_info(_Request, ProtocolState) ->
    {noreply, ProtocolState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, ProtocolState, _Extra) ->
    {ok, ProtocolState}.

%% Helper methods %%

build_header(SrcAddress, DestAddress, Protocol, PayloadSize, TTL, MayFragment, ProtocolState) ->
    Identification = ProtocolState#nsime_ipv4_protocol_state.identification,
    Header = nsime_ipv4_header:set_payload_size(
        #nsime_ipv4_header{
            source_address = SrcAddress,
            destination_address = DestAddress,
            protocol = Protocol,
            ttl = TTL,
            identification = Identification,
            calculate_checksum = nsime_config:checksum_enabled()
        },
        PayloadSize
    ),
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        identification = Identification + 1
    },
    case MayFragment of
        true ->
            {nsime_ipv4_header:set_may_fragment(Header), NewProtocolState};
        false ->
            {nsime_ipv4_header:set_dont_fragment(Header), NewProtocolState}
    end.

send_real_out(Route, Packet, Ipv4Header, ProtocolState) ->
    case is_record(Route, nsime_ipv4_route) of
        false ->
            {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
            NewArgs = lists:flatten([Args | [Ipv4Header, Packet, drop_no_route, self(), none]]),
            erlang:apply(Mod, Fun, NewArgs),
            {reply, ok, ProtocolState};
        true ->
            Ipv4HeaderBinary = nsime_ipv4_header:serialize(Ipv4Header),
            Data = Packet#nsime_packet.data,
            NewPacket = Packet#nsime_packet{data = <<Ipv4HeaderBinary/binary, Data/binary>>},
            DevicePid = Route#nsime_ipv4_route.output_device,
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
            case Route#nsime_ipv4_route.gateway == nsime_ipv4_address:get_zero() of
                false ->
                    case nsime_ipv4_interface:is_up(InterfacePid) of
                        true ->
                            case
                                NewPacket#nsime_packet.size >
                                nsime_netdevice:get_mtu(nsime_ipv4_interface:get_device(InterfacePid))
                            of
                                true ->
                                    erlang:error(ipv4_fragmentation_not_supported);
                                false ->
                                    {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.transmit_trace,
                                    NewArgs = lists:flatten([Args | [NewPacket, self(), InterfacePid]]),
                                    erlang:apply(Mod, Fun, NewArgs),
                                    nsime_ipv4_interface:send(InterfacePid, NewPacket, Route#nsime_ipv4_route.gateway),
                                    {reply, ok, ProtocolState}
                            end;
                        false ->
                            {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                            NewArgs = lists:flatten([Args | [Ipv4Header, Packet, drop_interface_down, self(), InterfacePid]]),
                            erlang:apply(Mod, Fun, NewArgs),
                            {reply, ok, ProtocolState}
                    end;
                true ->
                    case nsime_ipv4_interface:is_up(InterfacePid) of
                        true ->
                            case
                                NewPacket#nsime_packet.size >
                                nsime_netdevice:get_mtu(nsime_ipv4_interface:get_device(InterfacePid))
                            of
                                true ->
                                    erlang:error(ipv4_fragmentation_not_supported);
                                false ->
                                    {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.transmit_trace,
                                    NewArgs = lists:flatten([Args | [NewPacket, self(), InterfacePid]]),
                                    erlang:apply(Mod, Fun, NewArgs),
                                    nsime_ipv4_interface:send(
                                        InterfacePid,
                                        NewPacket,
                                        nsime_ipv4_header:get_destination_address(
                                            Ipv4Header
                                        )
                                    ),
                                    {reply, ok, ProtocolState}
                            end;
                        false ->
                            {Mod, Fun, Args} = ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                            NewArgs = lists:flatten([Args | [Ipv4Header, Packet, drop_interface_down, self(), InterfacePid]]),
                            erlang:apply(Mod, Fun, NewArgs),
                            {reply, ok, ProtocolState}
                    end
            end
    end.
