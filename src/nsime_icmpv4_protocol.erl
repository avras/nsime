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

%% Purpose : ICMPv4 protocol module
%% Author : Saravanan Vijayakumaran

-module(nsime_icmpv4_protocol).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_ipv4_route.hrl").
-include("nsime_icmpv4_headers.hrl").
-include("nsime_icmpv4_protocol_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, set_node/2, protocol_number/0,
         recv/4, send_time_exceeded_ttl/3, send_dest_unreach_port/3,
         set_ipv4_down_target/2, get_ipv4_down_target/1]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(ProtocolPid) ->
    gen_server:call(ProtocolPid, terminate).

set_node(ProtocolPid, NodePid) ->
    gen_server:call(ProtocolPid, {set_node, NodePid}).

protocol_number() ->
    ?ICMPv4_PROTOCOL_NUMBER.

recv(ProtocolPid, Packet, Ipv4Header, Interface) ->
    gen_server:call(ProtocolPid, {recv, Packet, Ipv4Header, Interface}).

send_time_exceeded_ttl(ProtocolPid, Ipv4Header, Packet) ->
    gen_server:call(ProtocolPid, {send_time_exceeded_ttl, Ipv4Header, Packet}).

send_dest_unreach_port(ProtocolPid, Ipv4Header, Packet) ->
    gen_server:call(ProtocolPid, {send_dest_unreach_port, Ipv4Header, Packet}).

set_ipv4_down_target(ProtocolPid, Callback) ->
    gen_server:call(ProtocolPid, {set_ipv4_down_target, Callback}).

get_ipv4_down_target(ProtocolPid) ->
    gen_server:call(ProtocolPid, get_ipv4_down_target).

init([]) ->
    ProtocolState = #nsime_icmpv4_protocol_state{},
    {ok, ProtocolState}.

handle_call({set_node, NodePid}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_icmpv4_protocol_state{node = NodePid},
    {reply, ok, NewProtocolState};

handle_call(protocol_number, _From, ProtocolState) ->
    {reply, ?ICMPv4_PROTOCOL_NUMBER, ProtocolState};

handle_call({recv, Packet, Ipv4Header, _Interface}, _From, ProtocolState) ->
    Data = Packet#nsime_packet.data,
    IcmpHeader = nsime_icmpv4_header:deserialize(Data),
    SrcAddress = nsime_ipv4_header:get_source_address(Ipv4Header),
    DestAddress = nsime_ipv4_header:get_destination_address(Ipv4Header),
    case IcmpHeader#nsime_icmpv4_header.type of
        ?ICMPv4_ECHO ->
            handle_echo(Packet, IcmpHeader, SrcAddress, DestAddress, ProtocolState),
            {reply, rx_ok, ProtocolState};
        ?ICMPv4_DEST_UNREACH ->
            handle_dest_unreach(Packet, IcmpHeader, SrcAddress, DestAddress, ProtocolState),
            {reply, rx_ok, ProtocolState};
        ?ICMPv4_TIME_EXCEEDED ->
            handle_time_exceeded(Packet, IcmpHeader, SrcAddress, DestAddress, ProtocolState),
            {reply, rx_ok, ProtocolState};
        _ ->
            {reply, rx_ok, ProtocolState}
    end;

handle_call({send_time_exceeded_ttl, Ipv4Header, Packet}, _From, ProtocolState) ->
    HeaderBinary = nsime_icmpv4_time_exceeded_header:serialize(
        nsime_icmpv4_time_exceeded_header:set_data(
            #nsime_icmpv4_time_exceeded_header{
              header = Ipv4Header
            },
            Packet
        )
    ),
    Data = Packet#nsime_packet.data,
    NewPacket = Packet#nsime_packet{
        data = <<HeaderBinary/binary, Data/binary>>
    },
    send_message(
        NewPacket,
        nsime_ipv4_header:get_source_address(Ipv4Header),
        ?ICMPv4_TIME_EXCEEDED,
        ?ICMPv4_TIME_TO_LIVE,
        ProtocolState
    ),
    {reply, ok, ProtocolState};

handle_call({send_dest_unreach_port, Ipv4Header, Packet}, _From, ProtocolState) ->
    send_dest_unreach(Ipv4Header, Packet, ?ICMPv4_PORT_UNREACHABLE, 0, ProtocolState),
    {reply, ok, ProtocolState};

handle_call({set_ipv4_down_target, Callback}, _From, ProtocolState) ->
    NewProtocolState = ProtocolState#nsime_icmpv4_protocol_state{
        ipv4_down_target = Callback
    },
    {reply, ok, NewProtocolState};

handle_call(get_ipv4_down_target, _From, ProtocolState) ->
    Callback = ProtocolState#nsime_icmpv4_protocol_state.ipv4_down_target,
    {reply, Callback, ProtocolState};

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

handle_echo(Packet, _IcmpHeader, SrcAddress, DestAddress, ProtocolState) ->
    <<EchoHeaderBinary:32, _Payload/binary>> = Packet#nsime_packet.data,
    ReplyPacket = #nsime_packet{data = EchoHeaderBinary, size = 4},
    send_message(ReplyPacket, DestAddress, SrcAddress, ?ICMPv4_ECHO_REPLY, 0, 0, ProtocolState).

handle_dest_unreach(Packet, IcmpHeader, SrcAddress, _DestAddress, ProtocolState) ->
    DestUnreachableHeader = nsime_icmpv4_dest_unreachable:deserialize(
        Packet#nsime_packet.data
    ),
    forward(
        SrcAddress,
        IcmpHeader,
        nsime_icmpv4_dest_unreachable:get_next_hop_mtu(DestUnreachableHeader),
        nsime_icmpv4_dest_unreachable:get_header(DestUnreachableHeader),
        nsime_icmpv4_dest_unreachable:get_data(DestUnreachableHeader),
        ProtocolState
    ).

handle_time_exceeded(Packet, IcmpHeader, SrcAddress, _DestAddress, ProtocolState) ->
    TimeExceededHeader = nsime_icmpv4_time_exceeded_header:deserialize(
        Packet#nsime_packet.data
    ),
    forward(
        SrcAddress,
        IcmpHeader,
        0,
        nsime_icmpv4_dest_unreachable:get_header(TimeExceededHeader),
        nsime_icmpv4_dest_unreachable:get_data(TimeExceededHeader),
        ProtocolState
    ).

forward(SrcAddress, IcmpHeader, Info, Ipv4Header, Payload, ProtocolState) ->
    NodePid = ProtocolState#nsime_icmpv4_protocol_state.node,
    Ipv4ProtocolPid = nsime_node:get_object(NodePid, nsime_ipv4_protocol),
    Layer4ProtocolPid = nsime_ipv4_protocol:get_protocol(
        Ipv4ProtocolPid,
        nsime_ipv4_header:get_protocol(Ipv4Header)
    ),
    case is_pid(Layer4ProtocolPid) of
        true ->
            nsime_layer4_protocol:recv_icmp(
                Layer4ProtocolPid,
                SrcAddress,
                nsime_ipv4_header:get_ttl(Ipv4Header),
                nsime_icmpv4_header:get_type(IcmpHeader),
                nsime_icmpv4_header:get_code(IcmpHeader),
                Info,
                nsime_ipv4_header:get_source_address(Ipv4Header),
                nsime_ipv4_header:get_destination_address(Ipv4Header),
                Payload
            );
        false ->
            ok
    end.

send_message(Packet, DestAddress, IcmpType, IcmpCode, ProtocolState) ->
    NodePid = ProtocolState#nsime_icmpv4_protocol_state.node,
    Ipv4ProtocolPid = nsime_node:get_object(NodePid, nsime_ipv4_protocol),
    Ipv4Header = #nsime_ipv4_header{
        destination_address = DestAddress,
        protocol = ?ICMPv4_PROTOCOL_NUMBER
    },
    {RoutingModule, RoutingState} = nsime_ipv4_protocol:get_routing_protocol(Ipv4ProtocolPid),
    InterfaceList = nsime_ipv4_protocol:get_interface_list(Ipv4ProtocolPid),
    case
    RoutingModule:route_output(
        RoutingState,
        Ipv4Header,
        none,
        InterfaceList
    )
    of
        {error_noroutetohost, undefined} ->
            none;
        {error_noterror, Route} ->
            send_message(
                Packet,
                Route#nsime_ipv4_route.source,
                DestAddress,
                IcmpType,
                IcmpCode,
                Route,
                ProtocolState
            )
    end.

send_message(Packet, SrcAddress, DestAddress, IcmpType, IcmpCode, Route, ProtocolState) ->
    HeaderBinary = nsime_icmpv4_header:serialize(
        #nsime_icmpv4_header{
            type = IcmpType,
            code = IcmpCode,
            calculate_checksum = nsime_config:checksum_enabled()
        }
    ),
    Data = Packet#nsime_packet.data,
    NewPacket = Packet#nsime_packet{
        data = <<HeaderBinary/binary, Data/binary>>
    },
    {Mod, Fun, Args} = ProtocolState#nsime_icmpv4_protocol_state.ipv4_down_target,
    NewArgs = lists:flatten([Args, [NewPacket, SrcAddress, DestAddress, ?ICMPv4_PROTOCOL_NUMBER, Route]]),
    erlang:apply(Mod, Fun, NewArgs).

send_dest_unreach(Ipv4Header, Packet, IcmpCode, NextHopMtu, ProtocolState) ->
    HeaderBinary = nsime_icmpv4_dest_unreachable_header:serialize(
        nsime_icmpv4_dest_unreachable_header:set_data(
            #nsime_icmpv4_dest_unreachable_header{
              header = Ipv4Header,
              next_hop_mtu = NextHopMtu
            },
            Packet
        )
    ),
    Data = Packet#nsime_packet.data,
    NewPacket = Packet#nsime_packet{
        data = <<HeaderBinary/binary, Data/binary>>
    },
    send_message(
        NewPacket,
        nsime_ipv4_header:get_source_address(Ipv4Header),
        ?ICMPv4_DEST_UNREACH,
        IcmpCode,
        ProtocolState
    ).
