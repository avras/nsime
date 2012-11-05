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

%% Purpose : UDP protocol module
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_protocol).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_udp_header.hrl").
-include("nsime_ip_endpoint_state.hrl").
-include("nsime_ip_endpoint_demux_state.hrl").
-include("nsime_udp_protocol_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, set_node/2, protocol_number/0,
         create_socket/1, get_sockets/1, allocate/2, allocate/3,
         allocate/4, allocate/6, deallocate/2, send/6, send/7, recv/4,
         recv_icmp/9, set_ipv4_down_target/2, get_ipv4_down_target/1,
         set_ipv6_down_target/2, get_ipv6_down_target/1]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(ProtocolPid) ->
    gen_server:call(ProtocolPid, terminate).

set_node(ProtocolPid, NodePid) ->
    gen_server:call(ProtocolPid, {set_node, NodePid}).

protocol_number() ->
    ?UDP_PROTOCOL_NUMBER.

create_socket(ProtocolPid) ->
    gen_server:call(ProtocolPid, create_socket).

get_sockets(ProtocolPid) ->
    gen_server:call(ProtocolPid, get_sockets).

allocate(ProtocolPid, Callbacks) ->
    gen_server:call(ProtocolPid, {allocate, Callbacks}).

allocate(ProtocolPid, AddressOrPort, Callbacks) ->
    gen_server:call(ProtocolPid, {allocate, AddressOrPort, Callbacks}).

allocate(ProtocolPid, Address, Port, Callbacks) ->
    gen_server:call(ProtocolPid, {allocate, Address, Port, Callbacks}).

allocate(ProtocolPid, LocalAddress, LocalPort, PeerAddress, PeerPort, Callbacks) ->
    gen_server:call(ProtocolPid, {allocate,
                                  LocalAddress,
                                  LocalPort,
                                  PeerAddress,
                                  PeerPort,
                                  Callbacks
                                 }).

deallocate(ProtocolPid, Endpoint) ->
    gen_server:call(ProtocolPid, {deallocate, Endpoint}).

send(ProtocolPid, Packet, SrcAddress, DestAddress, SrcPort, DestPort) ->
    gen_server:call(ProtocolPid, {send,
                                  Packet,
                                  SrcAddress,
                                  DestAddress,
                                  SrcPort,
                                  DestPort,
                                  undefined
                                 }).

send(ProtocolPid, Packet, SrcAddress, DestAddress, SrcPort, DestPort, Route) ->
    gen_server:call(ProtocolPid, {send,
                                  Packet,
                                  SrcAddress,
                                  DestAddress,
                                  SrcPort,
                                  DestPort,
                                  Route
                                 }).

recv(ProtocolPid, Packet, Header, Interface) ->
    gen_server:call(ProtocolPid, {recv, Packet, Header, Interface}).

recv_icmp(
    ProtocolPid,
    IcmpSource,
    IcmpTTL,
    IcmpType,
    IcmpCode,
    IcmpInfo,
    PayloadSource,
    PayloadDestination,
    Payload
) ->
    gen_server:call(ProtocolPid, {recv_icmp,
                                  IcmpSource,
                                  IcmpTTL,
                                  IcmpType,
                                  IcmpCode,
                                  IcmpInfo,
                                  PayloadSource,
                                  PayloadDestination,
                                  Payload
                                 }).

set_ipv4_down_target(ProtocolPid, Callback) ->
    gen_server:call(ProtocolPid, {set_ipv4_down_target, Callback}).

get_ipv4_down_target(ProtocolPid) ->
    gen_server:call(ProtocolPid, get_ipv4_down_target).

set_ipv6_down_target(ProtocolPid, Callback) ->
    gen_server:call(ProtocolPid, {set_ipv6_down_target, Callback}).

get_ipv6_down_target(ProtocolPid) ->
    gen_server:call(ProtocolPid, get_ipv6_down_target).

init([]) ->
    ProtocolState = #nsime_udp_protocol_state{
        ipv4_endpoints_demux = nsime_ip_endpoint_demux:create(),
        checksum_enabled = nsime_config:checksum_enabled()
    },
    {ok, ProtocolState}.

handle_call({set_node, NodePid}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{node = NodePid},
    {reply, ok, NewProtocolState};

handle_call(protocol_number, _From, ProtocolState) ->
    {reply, ?UDP_PROTOCOL_NUMBER, ProtocolState};

handle_call(create_socket, _From, ProtocolState) ->
    SocketPid = nsime_udp_socket:create(),
    nsime_udp_socket:set_node(SocketPid, ProtocolState#nsime_udp_protocol_state.node),
    nsime_udp_socket:set_udp_protocol(SocketPid, self()),
    Sockets = ProtocolState#nsime_udp_protocol_state.sockets,
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{
        sockets = [SocketPid | Sockets]
    },
    {reply, SocketPid, NewProtocolState};

handle_call(get_sockets, _From, ProtocolState) ->
    Sockets = ProtocolState#nsime_udp_protocol_state.sockets,
    {reply, Sockets, ProtocolState};

handle_call({allocate, Callbacks}, _From, ProtocolState) ->
    DemuxState = ProtocolState#nsime_udp_protocol_state.ipv4_endpoints_demux,
    {Endpoint, NewDemuxState} = nsime_ip_endpoint_demux:allocate(DemuxState, Callbacks),
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{
        ipv4_endpoints_demux = NewDemuxState
    },
    {reply, Endpoint, NewProtocolState};

handle_call({allocate, AddressOrPort, Callbacks}, _From, ProtocolState) ->
    DemuxState = ProtocolState#nsime_udp_protocol_state.ipv4_endpoints_demux,
    {Endpoint, NewDemuxState} = nsime_ip_endpoint_demux:allocate(DemuxState, AddressOrPort, Callbacks),
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{
        ipv4_endpoints_demux = NewDemuxState
    },
    {reply, Endpoint, NewProtocolState};

handle_call({allocate, Address, Port, Callbacks}, _From, ProtocolState) ->
    DemuxState = ProtocolState#nsime_udp_protocol_state.ipv4_endpoints_demux,
    {Endpoint, NewDemuxState} = nsime_ip_endpoint_demux:allocate(DemuxState, Address, Port, Callbacks),
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{
        ipv4_endpoints_demux = NewDemuxState
    },
    {reply, Endpoint, NewProtocolState};

handle_call(
    {allocate, LocalAddress, LocalPort, PeerAddress, PeerPort, Callbacks},
    _From, ProtocolState
) ->
    DemuxState = ProtocolState#nsime_udp_protocol_state.ipv4_endpoints_demux,
    {Endpoint, NewDemuxState} = nsime_ip_endpoint_demux:allocate(
        DemuxState,
        LocalAddress,
        LocalPort,
        PeerAddress,
        PeerPort,
        Callbacks
    ),
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{
        ipv4_endpoints_demux = NewDemuxState
    },
    {reply, Endpoint, NewProtocolState};

handle_call({deallocate, Endpoint}, _From, ProtocolState) ->
    DemuxState = ProtocolState#nsime_udp_protocol_state.ipv4_endpoints_demux,
    NewDemuxState = nsime_ip_endpoint_demux:deallocate(DemuxState, Endpoint),
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{
        ipv4_endpoints_demux = NewDemuxState
    },
    {reply, ok, NewProtocolState};

handle_call(
    {send,
        Packet,
        SrcAddress,
        DestAddress,
        SrcPort,
        DestPort,
        Route
    },
    _From,
    ProtocolState
) ->
    Length = Packet#nsime_packet.size + 8,
    Data = Packet#nsime_packet.data,
    Checksum =
    case ProtocolState#nsime_udp_protocol_state.checksum_enabled of
        true ->
            nsime_udp_header:calculate_checksum(
                Data,
                SrcAddress,
                DestAddress,
                SrcPort,
                DestPort,
                Length,
                ?UDP_PROTOCOL_NUMBER
            );
        false ->
            0
    end,
    UdpHeader = nsime_udp_header:serialize(
        SrcPort,
        DestPort,
        Length,
        Checksum
    ),
    NewPacket = Packet#nsime_packet{
        size = Length,
        data = <<UdpHeader/binary, Data/binary>>
    },
    {Mod, Fun, Args} = ProtocolState#nsime_udp_protocol_state.ipv4_down_target,
    NewArgs = lists:flatten([Args | [NewPacket, SrcAddress, DestAddress, ?UDP_PROTOCOL_NUMBER, Route]]),
    erlang:apply(Mod, Fun, NewArgs),
    {reply, ok, ProtocolState};

handle_call({recv, Packet, Ipv4Header, Interface}, _From, ProtocolState) ->
    Data = Packet#nsime_packet.data,
    <<UdpHeaderBinary:64, Payload/binary>> = Data,
    UdpHeader = nsime_udp_header:deserialize(<<UdpHeaderBinary:64>>),
    DestAddress = nsime_ipv4_header:get_destination_address(Ipv4Header),
    SrcAddress = nsime_ipv4_header:get_source_address(Ipv4Header),
    Checksum =
    case ProtocolState#nsime_udp_protocol_state.checksum_enabled of
        true ->
            nsime_udp_header:calculate_checksum(
                <<Payload/binary>>,
                SrcAddress,
                DestAddress,
                UdpHeader#nsime_udp_header.source_port,
                UdpHeader#nsime_udp_header.destination_port,
                UdpHeader#nsime_udp_header.length,
                ?UDP_PROTOCOL_NUMBER
            );
        false ->
            0
    end,
    case (Checksum == UdpHeader#nsime_udp_header.checksum) of
        false ->
            {reply, rx_csum_failed, ProtocolState};
        true ->
            DemuxState = ProtocolState#nsime_udp_protocol_state.ipv4_endpoints_demux,
            MatchingEndpoints = nsime_ip_endpoint_demux:lookup(
                DemuxState,
                DestAddress,
                UdpHeader#nsime_udp_header.destination_port,
                SrcAddress,
                UdpHeader#nsime_udp_header.source_port,
                Interface
            ),
            case length(MatchingEndpoints) > 0 of
                false ->
                    {reply, rx_endpoint_unreach, ProtocolState};
                true ->
                    NewPacket = Packet#nsime_packet{
                        size = byte_size(Data) - 8,
                        data = Payload
                    },
                    lists:foreach(
                        fun(E) ->
                            nsime_ip_endpoint:forward_up(
                                E,
                                NewPacket,
                                Ipv4Header,
                                UdpHeader#nsime_udp_header.source_port,
                                Interface
                            )
                        end,
                        MatchingEndpoints
                    ),
                    {reply, rx_ok, ProtocolState}
            end
    end;

handle_call(
    {recv_icmp,
        _IcmpSource,
        _IcmpTTL,
        _IcmpType,
        _IcmpCode,
        _IcmpInfo,
        _PayloadSource,
        _PayloadDestination,
        _Payload
    },
    _From,
    ProtocolState
) ->
    {reply, ok, ProtocolState};

handle_call({set_ipv4_down_target, Callback}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{
        ipv4_down_target = Callback
    },
    {reply, ok, NewProtocolState};

handle_call(get_ipv4_down_target, _From, ProtocolState) ->
    Callback = ProtocolState#nsime_udp_protocol_state.ipv4_down_target,
    {reply, Callback, ProtocolState};

handle_call({set_ipv6_down_target, Callback}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{
        ipv6_down_target = Callback
    },
    {reply, ok, NewProtocolState};

handle_call(get_ipv6_down_target, _From, ProtocolState) ->
    Callback = ProtocolState#nsime_udp_protocol_state.ipv6_down_target,
    {reply, Callback, ProtocolState};

handle_call(terminate, _From, ProtocolState) ->
    Sockets = ProtocolState#nsime_udp_protocol_state.sockets,
    lists:foreach(
        fun(S) ->
            nsime_udp_socket:destroy(S)
        end,
        Sockets
    ),
    NewProtocolState = ProtocolState#nsime_udp_protocol_state{
        ipv4_endpoints_demux = undefined,
        sockets = []
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
