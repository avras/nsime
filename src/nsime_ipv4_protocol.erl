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
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").
-include("nsime_ipv4_routing_table_entry.hrl").
-include("nsime_ipv4_static_routing_state.hrl").
-include("nsime_ipv4_list_routing_state.hrl").
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
         recv/7, send/6, send_with_header/4,
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

set_routing_protocol(ProtocolPid, {RoutingModule, RoutingState}) ->
    gen_server:call(ProtocolPid, {set_routing_protocol, {RoutingModule, RoutingState}}).

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

recv(ProtocolPid, DevicePid, Packet, Protocol, FromAddress, ToAddress, PacketType) ->
    gen_server:call(ProtocolPid, {recv, DevicePid, Packet, Protocol, FromAddress, ToAddress, PacketType}).

send(ProtocolPid, Packet, SrcAddress, DestAddress, Protocol, Route) ->
    case gen_server:call(ProtocolPid, {send, Packet, SrcAddress, DestAddress, Protocol, Route}) of
        nsime_routing_protocol_undefined ->
            erlang:error(nsime_routing_protocol_undefined);
        nsime_situation_not_implemented ->
            erlang:error(nsime_situation_not_implemented);
        ok ->
            ok
    end.

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

is_destination_address(ProtocolPid, Ipv4Address, Interface) ->
    gen_server:call(ProtocolPid, {is_destination_address, Ipv4Address, Interface}).

add_interface_address(ProtocolPid, InterfaceId, InterfaceAddress) ->
    gen_server:call(ProtocolPid, {add_interface_address, InterfaceId, InterfaceAddress}).

remove_interface_address(ProtocolPid, InterfaceId, InterfaceAddress) ->
    gen_server:call(ProtocolPid, {remove_interface_address, InterfaceId, InterfaceAddress}).

get_interface_address_list(ProtocolPid, InterfaceId) ->
    gen_server:call(ProtocolPid, {get_interface_address_list, InterfaceId}).

select_source_address(ProtocolPid, DevicePid, DestAddress, InterfaceAddressScope) ->
    gen_server:call(ProtocolPid, {select_source_address, DevicePid, DestAddress, InterfaceAddressScope}).

set_metric(ProtocolPid, InterfaceId, Metric) ->
    gen_server:call(ProtocolPid, {set_metric, InterfaceId, Metric}).

get_metric(ProtocolPid, InterfaceId) ->
    gen_server:call(ProtocolPid, {get_metric, InterfaceId}).

get_mtu(ProtocolPid, InterfaceId) ->
    gen_server:call(ProtocolPid, {get_mtu, InterfaceId}).

is_up(ProtocolPid, InterfaceId) ->
    gen_server:call(ProtocolPid, {is_up, InterfaceId}).

set_up(ProtocolPid, InterfaceId) ->
    gen_server:call(ProtocolPid, {set_up, InterfaceId}).

set_down(ProtocolPid, InterfaceId) ->
    gen_server:call(ProtocolPid, {set_down, InterfaceId}).

is_forwarding(ProtocolPid, InterfaceId) ->
    gen_server:call(ProtocolPid, {is_forwarding, InterfaceId}).

set_forwarding(ProtocolPid, InterfaceId, Forwarding) ->
    gen_server:call(ProtocolPid, {set_forwarding, InterfaceId, Forwarding}).

get_netdevice(ProtocolPid, InterfaceId) ->
    gen_server:call(ProtocolPid, {get_netdevice, InterfaceId}).

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
    case gen_server:call(ProtocolPid, {ip_forward, Route, Packet, Ipv4Header}) of
        ipv4_fragmentation_not_supported ->
            erlang:error(ipv4_fragmentation_not_supported);
        icmpv4_protocol_not_found ->
            erlang:error(icmpv4_protocol_not_found);
        ok ->
            ok
    end.

local_deliver(ProtocolPid, Packet, Ipv4Header, Interface) ->
    gen_server:call(ProtocolPid, {local_deliver, Packet, Ipv4Header, Interface}).

route_input_error(ProtocolPid, Packet, Ipv4Header) ->
    gen_server:call(ProtocolPid, {route_input_error, Packet, Ipv4Header}).

init([]) ->
    ProtocolState = #nsime_ipv4_protocol_state{},
    {ok, ProtocolState}.

handle_call({set_node, NodePid}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{node = NodePid},
    nsime_node:add_object(NodePid, nsime_ipv4_protocol, self()),
    {reply, ok, NewProtocolState};

handle_call(protocol_number, _From, ProtocolState) ->
    {reply, ?IPv4_PROTOCOL_NUMBER, ProtocolState};

handle_call({set_routing_protocol, {RoutingModule, RoutingState}}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    NewRoutingState = RoutingModule:populate_network_routes(RoutingState, InterfaceList),
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        routing_protocol = {RoutingModule, NewRoutingState}
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

handle_call({recv, DevicePid, Packet, _Protocol, _FromAddress, _ToAddress, _PacketType}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    Interface = lists:foldl(
        fun(I, Found) ->
            case is_record(Found, nsime_ipv4_interface_state) of
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
    case nsime_ipv4_interface:is_up(Interface) of
        false ->
            DataSizeBits = (Packet#nsime_packet.size - 20)*8,
            <<Ipv4HeaderBinary:160, Data:DataSizeBits>> = Packet#nsime_packet.data,
            Ipv4Header = nsime_ipv4_header:deserialize(<<Ipv4HeaderBinary:160>>),
            NewPacket = Packet#nsime_packet{data = <<Data:DataSizeBits>>},
            nsime_callback:apply(
                ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                [Ipv4Header, NewPacket, drop_interface_down, self(), Interface]
            ),
            {reply, ok, ProtocolState};
        true ->
            nsime_callback:apply(
                ProtocolState#nsime_ipv4_protocol_state.receive_trace,
                [Packet, self(), Interface]
            ),
            DataSizeBits = (Packet#nsime_packet.size - 20)*8,
            <<Ipv4HeaderBinary:160, Data:DataSizeBits>> = Packet#nsime_packet.data,
            Ipv4Header = nsime_ipv4_header:deserialize(<<Ipv4HeaderBinary:160>>),
            NewIpv4Header = case nsime_config:checksum_enabled() of
                true ->
                    nsime_ipv4_header:enable_checksum(Ipv4Header);
                false ->
                    Ipv4Header
            end,
            NewPacket = Packet#nsime_packet{data = <<Data:DataSizeBits>>},
            case nsime_config:checksum_enabled() and not(nsime_ipv4_header:is_checksum_ok(NewIpv4Header)) of
                true ->
                    nsime_callback:apply(
                        ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                        [NewIpv4Header, NewPacket, drop_bad_checksum, self(), Interface]
                    ),
                    {reply, ok, ProtocolState};
                false ->
                    {RoutingModule, RoutingState} = ProtocolState#nsime_ipv4_protocol_state.routing_protocol,
                    case
                    RoutingModule:route_input(
                        RoutingState,
                        NewPacket,
                        NewIpv4Header,
                        DevicePid,
                        Interface,
                        {?MODULE, ip_forward, [self()]},
                        none,
                        {?MODULE, local_deliver, [self()]},
                        ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                        InterfaceList
                    )
                    of
                        false ->
                            nsime_callback:apply(
                                ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                                [Ipv4Header, NewPacket, drop_no_route, self(), Interface]
                            ),
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
                    nsime_callback:apply(
                        NewProtocolState#nsime_ipv4_protocol_state.send_outgoing_trace,
                        [Ipv4Header, NewPacket, I]
                    ),
                    Data = NewPacket#nsime_packet.data,
                    Ipv4HeaderBinary = nsime_ipv4_header:serialize(Ipv4Header),
                    NewerPacket = NewPacket#nsime_packet{
                        data = <<Ipv4HeaderBinary/binary, Data/binary>>,
                        size = Ipv4Header#nsime_ipv4_header.total_length
                    },
                    nsime_callback:apply(
                        NewProtocolState#nsime_ipv4_protocol_state.transmit_trace,
                        [NewerPacket, self(), I]
                    ),
                    nsime_ipv4_interface:send(I, NewerPacket, DestAddress, self())
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
                                                    nsime_callback:apply(
                                                        NewProtocolState#nsime_ipv4_protocol_state.send_outgoing_trace,
                                                        [Ipv4Header, NewPacket, I]
                                                    ),
                                                    Data = NewPacket#nsime_packet.data,
                                                    Ipv4HeaderBinary = nsime_ipv4_header:serialize(Ipv4Header),
                                                    NewerPacket = NewPacket#nsime_packet{
                                                        data = <<Ipv4HeaderBinary/binary, Data/binary>>,
                                                        size = Ipv4Header#nsime_ipv4_header.total_length
                                                    },
                                                    nsime_callback:apply(
                                                        NewProtocolState#nsime_ipv4_protocol_state.transmit_trace,
                                                        [NewerPacket, self(), I]
                                                    ),
                                                    nsime_ipv4_interface:send(I, NewerPacket, DestAddress, self()),
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
                            case (Route#nsime_ipv4_route.gateway =/= undefined) of
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
                                    Interface = lists:foldl(
                                        fun(I, Match) ->
                                            case is_record(Match, nsime_ipv4_interface_state) of
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
                                    nsime_callback:apply(
                                        NewProtocolState#nsime_ipv4_protocol_state.send_outgoing_trace,
                                        [Ipv4Header, NewPacket, Interface]
                                    ),
                                    send_real_out(Route, NewPacket, Ipv4Header, NewProtocolState);
                                false ->
                                    {reply, nsime_situation_not_implemented, ProtocolState}
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
                            case ProtocolState#nsime_ipv4_protocol_state.routing_protocol of
                                {RoutingModule, RoutingState} ->
                                    case RoutingModule:route_output(RoutingState, Ipv4Header, none, InterfaceList) of
                                        {error_noterror, NewRoute} ->
                                            DevicePid = NewRoute#nsime_ipv4_route.output_device,
                                            Interface = lists:foldl(
                                                fun(I, Match) ->
                                                    case is_record(Match, nsime_ipv4_interface_state) of
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
                                            nsime_callback:apply(
                                                NewProtocolState#nsime_ipv4_protocol_state.send_outgoing_trace,
                                                [Ipv4Header, NewPacket, Interface]
                                            ),
                                            send_real_out(NewRoute, NewPacket, Ipv4Header, NewProtocolState);
                                        {error_noroutetohost, undefined} ->
                                            nsime_callback:apply(
                                                ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                                                [Ipv4Header, NewPacket, drop_no_route, self(), none]
                                            ),
                                            {reply, ok, NewProtocolState}
                                    end;
                                undefined ->
                                    {reply, nsime_routing_protocol_undefined, NewProtocolState}
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
    NewInterface = nsime_ipv4_interface:set_forwarding(
        nsime_ipv4_interface:set_device(
            nsime_ipv4_interface:create(),
            DevicePid
        ),
        ProtocolState#nsime_ipv4_protocol_state.ip_forward
    ),
    nsime_netdevice:set_interface(DevicePid, nsime_ipv4_interface:get_id(NewInterface)),
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        interfaces = [NewInterface | InterfaceList]
    },
    {reply, NewInterface, NewProtocolState};

handle_call(get_interface_list, _From, ProtocolState) ->
    {reply, ProtocolState#nsime_ipv4_protocol_state.interfaces, ProtocolState};

handle_call({get_interface_for_address, Address}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    Interface = lists:foldl(
        fun(I, Match) ->
            case is_record(Match, nsime_ipv4_interface_state) of
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
    {reply, Interface, ProtocolState};

handle_call({get_interface_for_prefix, Address, Mask}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    Interface = lists:foldl(
        fun(I, Match) ->
            case is_record(Match, nsime_ipv4_interface_state) of
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
    {reply, Interface, ProtocolState};

handle_call({get_interface_for_device, DevicePid}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    Interface = lists:foldl(
        fun(I, Match) ->
            case is_record(Match, nsime_ipv4_interface_state) of
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
    {reply, Interface, ProtocolState};

handle_call({is_destination_address, Ipv4Address, Interface}, _From, ProtocolState) ->
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
                            {reply, IsMatch2, ProtocolState}
                    end
            end
    end;

handle_call({add_interface_address, InterfaceId, InterfaceAddress}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    NewInterface = nsime_ipv4_interface:add_address(MatchingInterface, InterfaceAddress),
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        interfaces = [NewInterface | RemainingInterfaceList]
    },
    {reply, ok, NewProtocolState};

handle_call({remove_interface_address, InterfaceId, InterfaceAddress}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    NewInterface = nsime_ipv4_interface:remove_address(MatchingInterface, InterfaceAddress),
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        interfaces = [NewInterface | RemainingInterfaceList]
    },
    {reply, ok, NewProtocolState};

handle_call({get_interface_address_list, InterfaceId}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], _RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    {reply, nsime_ipv4_interface:get_address_list(MatchingInterface), ProtocolState};

handle_call({select_source_address, _DevicePid, _DestAddress, _InterfaceAddressScope}, _From, ProtocolState) ->
    {reply, ok, ProtocolState};

handle_call({set_metric, InterfaceId, Metric}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    NewInterface = nsime_ipv4_interface:set_metric(MatchingInterface, Metric),
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        interfaces = [NewInterface | RemainingInterfaceList]
    },
    {reply, ok, NewProtocolState};

handle_call({get_metric, InterfaceId}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], _RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    {reply, nsime_ipv4_interface:get_metric(MatchingInterface), ProtocolState};

handle_call({get_mtu, InterfaceId}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], _RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    Mtu = nsime_netdevice:get_mtu(nsime_ipv4_interface:get_device(MatchingInterface)),
    {reply, Mtu, ProtocolState};

handle_call({is_up, InterfaceId}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], _RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    {reply, nsime_ipv4_interface:is_up(MatchingInterface), ProtocolState};

handle_call({set_up, InterfaceId}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    NewInterface = nsime_ipv4_interface:set_up(MatchingInterface),
    NewInterfaceList = [NewInterface | RemainingInterfaceList],
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        interfaces = NewInterfaceList
    },
    case ProtocolState#nsime_ipv4_protocol_state.routing_protocol of
        {RoutingModule, RoutingState} ->
            RoutingModule:notify_interface_up(
                RoutingState,
                InterfaceId,
                NewInterfaceList
            ),
            {reply, ok, NewProtocolState};
        undefined ->
            {reply, ok, NewProtocolState}
    end;

handle_call({set_down, InterfaceId}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    NewInterface = nsime_ipv4_interface:set_down(MatchingInterface),
    NewInterfaceList = [NewInterface | RemainingInterfaceList],
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        interfaces = NewInterfaceList
    },
    case ProtocolState#nsime_ipv4_protocol_state.routing_protocol of
        {RoutingModule, RoutingState} ->
            RoutingModule:notify_interface_down(
                RoutingState,
                InterfaceId,
                NewInterfaceList
            ),
            {reply, ok, NewProtocolState};
        undefined ->
            {reply, ok, NewProtocolState}
    end;


handle_call({is_forwarding, InterfaceId}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], _RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    {reply, nsime_ipv4_interface:is_forwarding(MatchingInterface), ProtocolState};

handle_call({set_forwarding, InterfaceId, Forwarding}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    NewInterface = nsime_ipv4_interface:set_forwarding(MatchingInterface, Forwarding),
    NewProtocolState = ProtocolState#nsime_ipv4_protocol_state{
        interfaces = [NewInterface | RemainingInterfaceList]
    },
    {reply, ok, NewProtocolState};

handle_call({get_netdevice, InterfaceId}, _From, ProtocolState) ->
    InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
    {[MatchingInterface], _RemainingInterfaceList} =
    lists:partition(
        fun(I) ->
            nsime_ipv4_interface:get_id(I) == InterfaceId
        end,
        InterfaceList
    ),
    {reply, nsime_ipv4_interface:get_device(MatchingInterface), ProtocolState};

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
    Interface = lists:foldl(
        fun(I, Match) ->
            case is_record(Match, nsime_ipv4_interface_state) of
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
    NewIpv4Header = nsime_ipv4_header:set_ttl(Ipv4Header, nsime_ipv4_header:get_ttl(Ipv4Header) - 1),
    case nsime_ipv4_header:get_ttl(NewIpv4Header) == 0 of
        true ->
            case
                (nsime_ipv4_header:get_protocol(NewIpv4Header) =/= nsime_icmpv4_protocol:protocol_number())
                and (
                        nsime_ipv4_address:is_broadcast(
                            nsime_ipv4_header:get_destination_address(
                                NewIpv4Header
                            )
                        ) == false
                    )
                and (
                        nsime_ipv4_address:is_multicast(
                            nsime_ipv4_header:get_destination_address(
                                NewIpv4Header
                            )
                        ) == false
                    )
            of
                true ->
                    ProtocolList = ProtocolState#nsime_ipv4_protocol_state.layer4_protocols,
                    IcmpPid =
                    case
                    lists:filter(
                        fun(P) ->
                            nsime_layer4_protocol:protocol_number(P) == nsime_icmpv4_protocol:protocol_number()
                        end,
                        ProtocolList
                    )
                    of
                        [] ->
                            undefined;
                        [FirstMatch|_] ->
                            FirstMatch
                    end,
                    case is_pid(IcmpPid) of
                        false ->
                            {reply, icmpv4_protocol_not_found, ProtocolState};
                        true ->
                            nsime_icmpv4_protocol:send_time_exceeded_ttl(IcmpPid, NewIpv4Header, Packet),
                            nsime_callback:apply(
                                ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                                [NewIpv4Header, Packet, drop_ttl_expired, self(), Interface]
                            ),
                            {reply, ok, ProtocolState}
                    end;
                false ->
                    nsime_callback:apply(
                        ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                        [NewIpv4Header, Packet, drop_ttl_expired, self(), Interface]
                    ),
                    {reply, ok, ProtocolState}
            end;
        false ->
            nsime_callback:apply(
                ProtocolState#nsime_ipv4_protocol_state.unicast_forward_trace,
                [NewIpv4Header, Packet, Interface]
            ),
            send_real_out(Route, Packet, NewIpv4Header, ProtocolState)
    end;

handle_call({local_deliver, Packet, Ipv4Header, Interface}, _From, ProtocolState) ->
    nsime_callback:apply(
        ProtocolState#nsime_ipv4_protocol_state.local_deliver_trace,
        [Ipv4Header, Packet, Interface]
    ),
    Layer4ProtocolPid = hd(lists:filter(
        fun(P) ->
            nsime_layer4_protocol:protocol_number(P) ==
            nsime_ipv4_header:get_protocol(Ipv4Header)
        end,
        ProtocolState#nsime_ipv4_protocol_state.layer4_protocols
    )),
    case is_pid(Layer4ProtocolPid) of
        true ->
            RxStatus = nsime_layer4_protocol:recv(Layer4ProtocolPid, Packet, Ipv4Header, Interface),
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
                            InterfaceAddressList = nsime_ipv4_interface:get_address_list(Interface),
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
                                    IcmpPidList = lists:filter(
                                        fun(P) ->
                                            nsime_layer4_protocol:protocol_number(P) == nsime_icmpv4_protocol:protocol_number()
                                        end,
                                        ProtocolState#nsime_ipv4_protocol_state.layer4_protocols
                                    ),
                                    case IcmpPidList of
                                        [] ->
                                            {reply, ok, ProtocolState};
                                        [IcmpPid | _] ->
                                            nsime_icmpv4_protocol:send_dest_unreach_port(IcmpPid, Ipv4Header, Packet),
                                            {reply, ok, ProtocolState}
                                    end;
                                true ->
                                    {reply, ok, ProtocolState}
                            end
                    end
            end;
        false ->
            {reply, ok, ProtocolState}
    end;

handle_call({route_input_error, Packet, Ipv4Header}, _From, ProtocolState) ->
    nsime_callback:apply(
        ProtocolState#nsime_ipv4_protocol_state.drop_trace,
        [Ipv4Header, Packet, drop_route_error, self(), none]
    ),
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
            nsime_callback:apply(
                ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                [Ipv4Header, Packet, drop_no_route, self(), none]
            ),
            {reply, ok, ProtocolState};
        true ->
            Ipv4HeaderBinary = nsime_ipv4_header:serialize(Ipv4Header),
            Data = Packet#nsime_packet.data,
            NewPacket = Packet#nsime_packet{
                data = <<Ipv4HeaderBinary/binary, Data/binary>>,
                size = byte_size(Data) + byte_size(Ipv4HeaderBinary)
            },
            DevicePid = Route#nsime_ipv4_route.output_device,
            InterfaceList = ProtocolState#nsime_ipv4_protocol_state.interfaces,
            Interface = lists:foldl(
                fun(I, Match) ->
                    case is_record(Match, nsime_ipv4_interface_state) of
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
                    case nsime_ipv4_interface:is_up(Interface) of
                        true ->
                            case
                                NewPacket#nsime_packet.size >
                                nsime_netdevice:get_mtu(nsime_ipv4_interface:get_device(Interface))
                            of
                                true ->
                                    {reply, ipv4_fragmentation_not_supported, ProtocolState};
                                false ->
                                    nsime_callback:apply(
                                        ProtocolState#nsime_ipv4_protocol_state.transmit_trace,
                                        [NewPacket, self(), Interface]
                                    ),
                                    nsime_ipv4_interface:send(Interface, NewPacket, Route#nsime_ipv4_route.gateway, self()),
                                    {reply, ok, ProtocolState}
                            end;
                        false ->
                            nsime_callback:apply(
                                ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                                [Ipv4Header, Packet, drop_interface_down, self(), Interface]
                            ),
                            {reply, ok, ProtocolState}
                    end;
                true ->
                    case is_record(Interface, nsime_ipv4_interface_state) andalso nsime_ipv4_interface:is_up(Interface) of
                        true ->
                            case
                                NewPacket#nsime_packet.size >
                                nsime_netdevice:get_mtu(nsime_ipv4_interface:get_device(Interface))
                            of
                                true ->
                                    {reply, ipv4_fragmentation_not_supported, ProtocolState};
                                false ->
                                    nsime_callback:apply(
                                        ProtocolState#nsime_ipv4_protocol_state.transmit_trace,
                                        [NewPacket, self(), Interface]
                                    ),
                                    nsime_ipv4_interface:send(
                                        Interface,
                                        NewPacket,
                                        nsime_ipv4_header:get_destination_address(Ipv4Header),
                                        self()
                                    ),
                                    {reply, ok, ProtocolState}
                            end;
                        false ->
                            nsime_callback:apply(
                                ProtocolState#nsime_ipv4_protocol_state.drop_trace,
                                [Ipv4Header, Packet, drop_interface_down, self(), Interface]
                            ),
                            {reply, ok, ProtocolState}
                    end
            end
    end.
