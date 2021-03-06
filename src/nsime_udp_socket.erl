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

%% Purpose : UDP socket module
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_socket).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_packet.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_ipv4_route.hrl").
-include("nsime_ipv4_packet_info_tag.hrl").
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").
-include("nsime_ip_endpoint_state.hrl").
-include("nsime_udp_socket_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, set_node/2, get_node/1,
         set_udp_protocol/2, get_socket_error/1, get_socket_type/1,
         bind/1, bind/2, close/1, shutdown_send/1, shutdown_receive/1,
         connect/2, listen/1, get_transmit_available/0, get_transmit_available/1,
         send/3, send_to/4, get_received_available/1, recv/3, recv_from/1, recv_from/3,
         multicast_join_group/3, multicast_leave_group/3, bind_to_netdevice/2,
         get_receive_buffer_size/1, set_receive_buffer_size/2,
         set_allow_broadcast/2, get_allow_broadcast/1, set_drop_trace_callback/2,
         set_icmp_callback/2, set_receive_callback/2,
         forward_icmp/6, forward_up/5, destroy_endpoint/1]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(SocketPid) ->
    gen_server:call(SocketPid, terminate).

set_node(SocketPid, NodePid) ->
    gen_server:call(SocketPid, {set_node, NodePid}).

get_node(SocketPid) ->
    gen_server:call(SocketPid, get_node).

set_udp_protocol(SocketPid, UdpProtocolPid) ->
    gen_server:call(SocketPid, {set_udp_protocol, UdpProtocolPid}).

get_socket_error(SocketPid) ->
    gen_server:call(SocketPid, get_socket_error).

get_socket_type(SocketPid) ->
    gen_server:call(SocketPid, get_socket_type).

bind(SocketPid) ->
    gen_server:call(SocketPid, bind).

bind(SocketPid, SocketAddress) ->
    gen_server:call(SocketPid, {bind, SocketAddress}).

close(SocketPid) ->
    gen_server:call(SocketPid, close).

shutdown_send(SocketPid) ->
    gen_server:call(SocketPid, shutdown_send).

shutdown_receive(SocketPid) ->
    gen_server:call(SocketPid, shutdown_receive).

connect(SocketPid, SocketAddress) ->
    gen_server:call(SocketPid, {connect, SocketAddress}).

listen(SocketPid) ->
    gen_server:call(SocketPid, listen).

get_transmit_available() ->
    ?MAX_IPv4_UDP_DATAGRAM_SIZE.

get_transmit_available(SocketPid) ->
    gen_server:call(SocketPid, get_transmit_available).

send(SocketPid, Packet, Flags) ->
    gen_server:call(SocketPid, {send, Packet, Flags}).

send_to(SocketPid, Packet, Flags, SocketAddress) ->
    gen_server:call(SocketPid, {send_to, Packet, Flags, SocketAddress}).

get_received_available(SocketPid) ->
    gen_server:call(SocketPid, get_received_available).

recv(SocketPid, MaxSize, Flags) ->
    gen_server:call(SocketPid, {recv, MaxSize, Flags}).

recv_from(SocketPid) ->
    recv_from(SocketPid, infinity, 0).

recv_from(SocketPid, MaxSize, Flags) ->
    case recv(SocketPid, MaxSize, Flags) of
        none ->
            none;
        Packet = #nsime_packet{tags = Tags} ->
            case proplists:get_value(socket_address_tag, Tags) of
                undefined ->
                    none;
                {Address, Port} ->
                    {Packet, {Address, Port}}
            end
    end.

multicast_join_group(_SocketPid, _InterfaceIndex, _GroupAddress) ->
    ok.

multicast_leave_group(_SocketPid, _InterfaceIndex, _GroupAddress) ->
    ok.

bind_to_netdevice(SocketPid, DevicePid) ->
    gen_server:call(SocketPid, {bind_to_netdevice, DevicePid}).

get_receive_buffer_size(SocketPid) ->
    gen_server:call(SocketPid, get_receive_buffer_size).

set_receive_buffer_size(SocketPid, BufferSize) ->
    gen_server:call(SocketPid, {set_receive_buffer_size, BufferSize}).

set_allow_broadcast(SocketPid, AllowBroadcast) ->
    gen_server:call(SocketPid, {set_allow_broadcast, AllowBroadcast}).

get_allow_broadcast(SocketPid) ->
    gen_server:call(SocketPid, get_allow_broadcast).

set_drop_trace_callback(SocketPid, Callback) ->
    gen_server:call(SocketPid, {set_drop_trace_callback, Callback}).

set_icmp_callback(SocketPid, Callback) ->
    gen_server:call(SocketPid, {set_icmp_callback, Callback}).

set_receive_callback(SocketPid, Callback) ->
    gen_server:call(SocketPid, {set_receive_callback, Callback}).

forward_icmp(SocketPid, Source, TTL, Type, Code, Info) ->
    gen_server:call(SocketPid, {forward_icmp, Source, TTL, Type, Code, Info}).

forward_up(SocketPid, Packet, Ipv4Header, Port, Interface) ->
    gen_server:call(SocketPid, {forward_up, Packet, Ipv4Header, Port, Interface}).

destroy_endpoint(SocketPid) ->
    gen_server:call(SocketPid, destroy_endpoint).

init([]) ->
    SocketState = #nsime_udp_socket_state{},
    {ok, SocketState}.

handle_call({set_node, NodePid}, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{node = NodePid},
    {reply, ok, NewSocketState};

handle_call(get_node, _From, SocketState) ->
    NodePid = SocketState#nsime_udp_socket_state.node,
    {reply, NodePid, SocketState};

handle_call({set_udp_protocol, UdpProtocolPid}, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{udp_protocol = UdpProtocolPid},
    {reply, ok, NewSocketState};

handle_call(get_socket_error, _From, SocketState) ->
    SocketError = SocketState#nsime_udp_socket_state.socket_error,
    {reply, SocketError, SocketState};

handle_call(get_socket_type, _From, SocketState) ->
    {reply, sock_dgram, SocketState};

handle_call(bind, _From, SocketState) ->
    UdpProtocolPid = SocketState#nsime_udp_socket_state.udp_protocol,
    Callbacks = set_endpoint_callbacks(self()),
    Endpoint = nsime_udp_protocol:allocate(UdpProtocolPid, Callbacks),
    NewSocketState = SocketState#nsime_udp_socket_state{ip_endpoint = Endpoint},
    {reply, ok, NewSocketState};

handle_call({bind, _SocketAddress = {Address, Port}}, _From, SocketState) ->
    UdpProtocolPid = SocketState#nsime_udp_socket_state.udp_protocol,
    Callbacks = set_endpoint_callbacks(self()),
    Endpoint = case {Address == nsime_ipv4_address:get_any(), Port == 0} of
        {true, true} ->
            nsime_udp_protocol:allocate(UdpProtocolPid, Callbacks);
        {true, false} ->
            nsime_udp_protocol:allocate(UdpProtocolPid, Port, Callbacks);
        {false, true} ->
            nsime_udp_protocol:allocate(UdpProtocolPid, Address, Callbacks);
        {false, false} ->
            nsime_udp_protocol:allocate(UdpProtocolPid, Address, Port, Callbacks)
    end,
    NewSocketState = SocketState#nsime_udp_socket_state{ip_endpoint = Endpoint},
    {reply, ok, NewSocketState};

handle_call(close, _From, SocketState) ->
    ShutdownReceive = SocketState#nsime_udp_socket_state.shutdown_receive,
    ShutdownSend = SocketState#nsime_udp_socket_state.shutdown_send,
    case ShutdownReceive and ShutdownSend of
        true ->
            NewSocketState = SocketState#nsime_udp_socket_state{
                socket_error = error_badf
            },
            {reply, error_badf, NewSocketState};
        false ->
            NewSocketState = SocketState#nsime_udp_socket_state{
                shutdown_send = true,
                shutdown_receive = true
            },
            {reply, ok, NewSocketState}
    end;

handle_call(shutdown_send, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        shutdown_send = true
    },
    {reply, ok, NewSocketState};

handle_call(shutdown_receive, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        shutdown_receive = true
    },
    {reply, ok, NewSocketState};

handle_call({connect, _SocketAddress = {Address, Port}}, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        default_address = Address,
        default_port = Port,
        connected = true
    },
    {reply, ok, NewSocketState};

handle_call(listen, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        socket_error = error_opnotsupp
    },
    {reply, error_opnotsupp, NewSocketState};

handle_call(get_transmit_available, _From, SocketState) ->
    {reply, ?MAX_IPv4_UDP_DATAGRAM_SIZE, SocketState};

handle_call({send, Packet, _Flags}, _From, SocketState) ->
    case SocketState#nsime_udp_socket_state.connected of
        false ->
            NewSocketState = SocketState#nsime_udp_socket_state{
                socket_error = error_notconn
            },
            {reply, error_notconn, NewSocketState};
        true ->
            case SocketState#nsime_udp_socket_state.shutdown_send of
                true ->
                    NewSocketState = SocketState#nsime_udp_socket_state{
                        socket_error = error_shutdown
                    },
                    {reply, error_shutdown, NewSocketState};
                false ->
                    case is_record(SocketState#nsime_udp_socket_state.ip_endpoint, nsime_ip_endpoint_state) of
                        true ->
                            do_send_to(Packet, SocketState);
                        false ->
                            UdpProtocolPid = SocketState#nsime_udp_socket_state.udp_protocol,
                            Callbacks = set_endpoint_callbacks(self()),
                            Endpoint = nsime_udp_protocol:allocate(UdpProtocolPid, Callbacks),
                            NewSocketState = SocketState#nsime_udp_socket_state{
                                ip_endpoint = Endpoint
                            },
                            do_send_to(Packet, NewSocketState)
                    end
            end
    end;

handle_call({send_to, Packet, _Flags, _SocketAddress = {Address, Port}}, _From, SocketState) ->
    case is_record(SocketState#nsime_udp_socket_state.ip_endpoint, nsime_ip_endpoint_state) of
        true ->
            do_send_to(Packet, Address, Port, SocketState);
        false ->
            UdpProtocolPid = SocketState#nsime_udp_socket_state.udp_protocol,
            Callbacks = set_endpoint_callbacks(self()),
            Endpoint = nsime_udp_protocol:allocate(UdpProtocolPid, Callbacks),
            NewSocketState = SocketState#nsime_udp_socket_state{
                ip_endpoint = Endpoint
            },
            do_send_to(Packet, Address, Port, NewSocketState)
    end;

handle_call(get_received_available, _From, SocketState) ->
    ReceivedAvailable = SocketState#nsime_udp_socket_state.received_available,
    {reply, ReceivedAvailable, SocketState};

handle_call({recv, MaxSize, _Flags}, _From, SocketState) ->
    DeliveryQueue = SocketState#nsime_udp_socket_state.delivery_queue,
    case queue:is_empty(DeliveryQueue) of
        true ->
            NewSocketState = SocketState#nsime_udp_socket_state{
                socket_error = error_again
            },
            {reply, none, NewSocketState};
        false ->
            {value, Packet} = queue:peek(DeliveryQueue),
            case Packet#nsime_packet.size =< MaxSize of
                true ->
                    ReceivedAvailable = SocketState#nsime_udp_socket_state.received_available,
                    {{value, Packet}, NewDeliveryQueue} = queue:out(DeliveryQueue),
                    NewSocketState = SocketState#nsime_udp_socket_state{
                        received_available = ReceivedAvailable - Packet#nsime_packet.size,
                        delivery_queue = NewDeliveryQueue
                    },
                    {reply, Packet, NewSocketState};
                false ->
                    {reply, none, SocketState}
            end
    end;

handle_call({bind_to_netdevice, DevicePid}, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        bound_netdevice = DevicePid
    },
    {reply, ok, NewSocketState};

handle_call(get_receive_buffer_size, _From, SocketState) ->
    AllowBroadcast = SocketState#nsime_udp_socket_state.receive_buffer_size,
    {reply, AllowBroadcast, SocketState};

handle_call({set_receive_buffer_size, BufferSize}, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        receive_buffer_size = BufferSize
    },
    {reply, ok, NewSocketState};

handle_call({set_allow_broadcast, AllowBroadcast}, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        allow_broadcast = AllowBroadcast
    },
    {reply, ok, NewSocketState};

handle_call(get_allow_broadcast, _From, SocketState) ->
    AllowBroadcast = SocketState#nsime_udp_socket_state.allow_broadcast,
    {reply, AllowBroadcast, SocketState};

handle_call({set_drop_trace_callback, Callback}, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        drop_trace_callback = Callback
    },
    {reply, ok, NewSocketState};

handle_call({set_icmp_callback, Callback}, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        icmp_callback = Callback
    },
    {reply, ok, NewSocketState};

handle_call({set_receive_callback, Callback}, _From, SocketState) ->
    NewSocketState = SocketState#nsime_udp_socket_state{
        receive_callback = Callback
    },
    {reply, ok, NewSocketState};

handle_call({forward_icmp, Source, TTL, Type, Code, Info}, _From, SocketState) ->
    case SocketState#nsime_udp_socket_state.icmp_callback of
        undefined ->
            {reply, none, SocketState};
        {Module, Function, Arguments} ->
            erlang:apply(
                Module,
                Function,
                lists:append(Arguments, [Source, TTL, Type, Code, Info])
            ),
            {reply, ok, SocketState}
    end;

handle_call({forward_up, Packet, Ipv4Header, Port, Interface}, _From, SocketState) ->
    case SocketState#nsime_udp_socket_state.shutdown_receive of
        true ->
            {reply, none, SocketState};
        false ->
            NewPacket = case SocketState#nsime_udp_socket_state.receive_packet_info of
                true ->
                    Tags1 = Packet#nsime_packet.tags,
                    PacketInfoTag = #nsime_ipv4_packet_info_tag{
                        interface = Interface
                    },
                    NewTags1 = [
                        {ipv4_packet_info_tag, PacketInfoTag} |
                        proplists:delete(ipv4_packet_info_tag, Tags1)
                    ],
                    Packet#nsime_packet{tags = NewTags1};
                false ->
                    Packet
            end,
            ReceivedAvailable = SocketState#nsime_udp_socket_state.received_available,
            ReceiveBufferSize = SocketState#nsime_udp_socket_state.receive_buffer_size,
            case (ReceivedAvailable + NewPacket#nsime_packet.size =< ReceiveBufferSize) of
                true ->
                    SocketAddress = {nsime_ipv4_header:get_source_address(Ipv4Header), Port},
                    Tags2 = NewPacket#nsime_packet.tags,
                    NewTags2 = [
                        {socket_address_tag, SocketAddress} |
                        proplists:delete(socket_address_tag, Tags2)
                    ],
                    NewerPacket = NewPacket#nsime_packet{
                        tags = NewTags2
                    },
                    DeliveryQueue = SocketState#nsime_udp_socket_state.delivery_queue,
                    NewDeliveryQueue = queue:in(NewerPacket, DeliveryQueue),
                    NewSocketState = SocketState#nsime_udp_socket_state{
                        delivery_queue = NewDeliveryQueue,
                        received_available = ReceivedAvailable + NewerPacket#nsime_packet.size
                    },
                    {Module, Function, Arguments} = SocketState#nsime_udp_socket_state.receive_callback,
                    Event = #nsime_event{
                        module = Module,
                        function = Function,
                        arguments = Arguments,
                        eventid = make_ref()
                    },
                    nsime_simulator:schedule_now(Event),
                    {reply, ok, NewSocketState};
                false ->
                    {Module, Function, Arguments} = SocketState#nsime_udp_socket_state.drop_trace_callback,
                    erlang:apply(Module, Function, lists:append(Arguments, [NewPacket])),
                    {reply, dropped, SocketState}
            end
    end;

handle_call(destroy_endpoint, _From, SocketState) ->
    UdpProtocolPid = SocketState#nsime_udp_socket_state.udp_protocol,
    Endpoint = SocketState#nsime_udp_socket_state.ip_endpoint,
    case is_record(Endpoint, nsime_ip_endpoint_state) of
        true ->
            nsime_udp_protocol:deallocate(UdpProtocolPid, Endpoint),
            NewSocketState = SocketState#nsime_udp_socket_state{ip_endpoint = undefined},
            {reply, ok, NewSocketState};
        false ->
            {reply, ok, SocketState}
        end;

handle_call(terminate, _From, SocketState) ->
    {stop, normal, stopped, SocketState}.

handle_cast(_Request, SocketState) ->
    {noreply, SocketState}.

handle_info(_Request, SocketState) ->
    {noreply, SocketState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, SocketState, _Extra) ->
    {ok, SocketState}.

%% Helper methods %%
set_endpoint_callbacks(UdpSocketPid) ->
    {
        {
            nsime_udp_socket,
            forward_up,
            [UdpSocketPid]
        },
        {
            nsime_udp_socket,
            forward_icmp,
            [UdpSocketPid]
        },
        {
            nsime_udp_socket,
            destroy_endpoint,
            [UdpSocketPid]
        }
    }.

do_send_to(
    Packet,
    SocketState = #nsime_udp_socket_state{
      default_address = DestAddress,
      default_port = DestPort
    }
) ->
    do_send_to(Packet, DestAddress, DestPort, SocketState).

do_send_to(Packet, DestAddress, DestPort, SocketState) ->
    UdpProtocolPid = SocketState#nsime_udp_socket_state.udp_protocol,
    Node = SocketState#nsime_udp_socket_state.node,
    Ipv4Protocol = nsime_node:get_object(Node, nsime_ipv4_protocol),
    case SocketState#nsime_udp_socket_state.shutdown_send of
        true ->
            NewSocketState = SocketState#nsime_udp_socket_state{
                socket_error = error_shutdown
            },
            {reply, error_shutdown, NewSocketState};
        false ->
            case
                Packet#nsime_packet.size >
                nsime_udp_socket:get_transmit_available()
            of
                true ->
                    NewSocketState = SocketState#nsime_udp_socket_state{
                        socket_error = error_msgsize
                    },
                    {reply, error_msgsize, NewSocketState};
                false ->
                    TTL = SocketState#nsime_udp_socket_state.ttl,
                    MulticastTTL = SocketState#nsime_udp_socket_state.multicast_ttl,
                    NewPacket =
                    case
                        (MulticastTTL =/= 0) and nsime_ipv4_address:is_multicast(DestAddress)
                    of
                        true ->
                            Tags1 = Packet#nsime_packet.tags,
                            NewTags1 = [
                                {socket_ip_ttl_tag, MulticastTTL} |
                                proplists:delete(socket_ip_ttl_tag, Tags1)
                            ],
                            Packet#nsime_packet{tags = NewTags1};
                        false ->
                            case
                                (
                                    (TTL =/= 0) and
                                    not(nsime_ipv4_address:is_multicast(DestAddress)) and
                                    not(nsime_ipv4_address:is_broadcast(DestAddress))
                                )
                            of
                                true ->
                                    Tags2 = Packet#nsime_packet.tags,
                                    NewTags2 = [
                                        {socket_ip_ttl_tag, TTL} |
                                        proplists:delete(socket_ip_ttl_tag, Tags2)
                                    ],
                                    Packet#nsime_packet{tags = NewTags2};
                                false ->
                                    Packet
                            end
                    end,
                    Tags = NewPacket#nsime_packet.tags,
                    NewerPacket =
                    case proplists:is_defined(socket_set_dont_fragment_tag, Tags) of
                        false ->
                            NewTags = [
                                {
                                    socket_set_dont_fragment_tag,
                                    SocketState#nsime_udp_socket_state.mtu_discover
                                } |
                                Tags
                            ],
                            NewPacket#nsime_packet{tags = NewTags};
                        true ->
                            NewPacket
                    end,
                    case nsime_ipv4_address:is_broadcast(DestAddress) of
                        true ->
                            case SocketState#nsime_udp_socket_state.allow_broadcast of
                                false ->
                                    NewSocketState = SocketState#nsime_udp_socket_state{
                                        socket_error = error_opnotsupp
                                    },
                                    {reply, error_opnotsupp, NewSocketState};
                                true ->
                                    InterfaceList = nsime_ipv4_protocol:get_interface_list(Ipv4Protocol),
                                    NewInterfaceList = lists:filter(
                                        fun(I) ->
                                            case nsime_ipv4_interface:get_address_list(I) of
                                                [] ->
                                                    false;
                                                [InterfaceAddress|_] ->
                                                    SrcAddress = nsime_ipv4_interface_address:get_local_address(
                                                        InterfaceAddress
                                                    ),
                                                    case SrcAddress == nsime_ipv4_address:get_loopback() of
                                                        true ->
                                                            false;
                                                        false ->
                                                            case
                                                            (
                                                                (is_pid(SocketState#nsime_udp_socket_state.bound_netdevice))
                                                                and
                                                                (
                                                                    SocketState#nsime_udp_socket_state.bound_netdevice
                                                                    =/= nsime_ipv4_interface:get_device(I)
                                                                )
                                                            )
                                                            of
                                                                true ->
                                                                    false;
                                                                false ->
                                                                    true
                                                            end
                                                    end
                                            end
                                        end,
                                        InterfaceList
                                    ),
                                    case NewInterfaceList of
                                        [] ->
                                            NewSocketState = SocketState#nsime_udp_socket_state{
                                                socket_error = error_noroutetohost
                                            },
                                            {reply, error_noroutetohost, NewSocketState};
                                        _ ->
                                            case
                                                lists:foldl(
                                                    fun(I, Error) ->
                                                        case nsime_ipv4_interface:get_address_list(I) of
                                                            [] ->
                                                                Error;
                                                            [InterfaceAddress | _] ->
                                                                Mask = nsime_ipv4_interface_address:get_mask(InterfaceAddress),
                                                                SrcAddress = nsime_ipv4_interface_address:get_local_address(
                                                                    InterfaceAddress
                                                                ),
                                                                FinalDestAddress =
                                                                case (Mask == nsime_ipv4_mask:get_ones()) of
                                                                    true ->
                                                                        DestAddress;
                                                                    false ->
                                                                        nsime_ipv4_address:get_subnet_directed_broadcast(
                                                                            SrcAddress,
                                                                            Mask
                                                                        )
                                                                end,
                                                                nsime_udp_protocol:send(
                                                                    UdpProtocolPid,
                                                                    NewerPacket,
                                                                    SrcAddress,
                                                                    FinalDestAddress,
                                                                    nsime_ip_endpoint:get_local_port(
                                                                        SocketState#nsime_udp_socket_state.ip_endpoint
                                                                    ),
                                                                    DestPort
                                                                ),
                                                                error_noterror
                                                        end
                                                    end,
                                                    error_noroutetohost,
                                                    NewInterfaceList
                                                )
                                            of
                                                error_noterror ->
                                                    {reply, NewerPacket#nsime_packet.size, SocketState};
                                                error_noroutetohost ->
                                                    NewSocketState = SocketState#nsime_udp_socket_state{
                                                        socket_error = error_noroutetohost
                                                    },
                                                    {reply, error_noroutetohost, NewSocketState}
                                            end
                                    end
                            end;
                        false ->
                            IpEndpoint = SocketState#nsime_udp_socket_state.ip_endpoint,
                            SrcAddress = nsime_ip_endpoint:get_local_address(IpEndpoint),
                            case SrcAddress =/= nsime_ipv4_address:get_any() of
                                true ->
                                    nsime_udp_protocol:send(
                                        UdpProtocolPid,
                                        NewerPacket,
                                        SrcAddress,
                                        DestAddress,
                                        nsime_ip_endpoint:get_local_port(IpEndpoint),
                                        DestPort
                                    ),
                                    {reply, NewerPacket#nsime_packet.size, SocketState};
                                false ->
                                    case nsime_ipv4_protocol:get_routing_protocol(Ipv4Protocol) of
                                        undefined ->
                                            NewSocketState = SocketState#nsime_udp_socket_state{
                                                socket_error = error_noroutetohost
                                            },
                                            {reply, error_noroutetohost, NewSocketState};
                                        {RoutingModule, RoutingState} ->
                                            Ipv4Header = #nsime_ipv4_header{
                                                    destination_address = DestAddress,
                                                    protocol = nsime_udp_protocol:protocol_number()
                                            },
                                            Netdevice = SocketState#nsime_udp_socket_state.bound_netdevice,
                                            case
                                                RoutingModule:route_output(
                                                    RoutingState,
                                                    Ipv4Header,
                                                    Netdevice,
                                                    nsime_ipv4_protocol:get_interface_list(Ipv4Protocol)
                                                )
                                            of
                                                {Error, undefined} ->
                                                    NewSocketState = SocketState#nsime_udp_socket_state{
                                                        socket_error = Error
                                                    },
                                                    {reply, Error, NewSocketState};
                                                {error_noterror, Route} ->
                                                    Result =
                                                    case SocketState#nsime_udp_socket_state.allow_broadcast of
                                                        false ->
                                                            Ipv4Protocol = nsime_node:get_object(
                                                                SocketState#nsime_udp_socket_state.node,
                                                                nsime_ipv4_protocol
                                                            ),
                                                            OutputInterface =
                                                                nsime_ipv4_protocol:get_interface_for_device(
                                                                    Ipv4Protocol,
                                                                    Route#nsime_ipv4_route.output_device
                                                                ),
                                                            InterfaceAddressList =
                                                            case is_record(OutputInterface, nsime_ipv4_interface_state) of
                                                                true ->
                                                                    nsime_ipv4_interface:get_address_list(OutputInterface);
                                                                false ->
                                                                    []
                                                            end,
                                                            lists:foldl(
                                                                fun(I, Acc) ->
                                                                    case Acc of
                                                                        error_opnotsupp ->
                                                                            error_opnotsupp;
                                                                        ok ->
                                                                            case
                                                                                DestAddress ==
                                                                                nsime_ipv4_interface_address:get_broadcast_address(
                                                                                    I
                                                                                )
                                                                            of
                                                                                true ->
                                                                                    error_opnotsupp;
                                                                                false ->
                                                                                    ok
                                                                            end
                                                                    end
                                                                end,
                                                                ok,
                                                                InterfaceAddressList
                                                            );
                                                        true ->
                                                            ok
                                                    end,
                                                    case Result of
                                                        error_opnotsupp ->
                                                            NewSocketState = SocketState#nsime_udp_socket_state{
                                                                socket_error = error_opnotsupp
                                                            },
                                                            {reply, error_opnotsupp, NewSocketState};
                                                        ok ->
                                                            nsime_udp_protocol:send(
                                                                UdpProtocolPid,
                                                                NewerPacket,
                                                                Route#nsime_ipv4_route.source,
                                                                DestAddress,
                                                                nsime_ip_endpoint:get_local_port(
                                                                    SocketState#nsime_udp_socket_state.ip_endpoint
                                                                ),
                                                                DestPort,
                                                                Route
                                                            ),
                                                            {reply, NewerPacket#nsime_packet.size, SocketState}
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end.
