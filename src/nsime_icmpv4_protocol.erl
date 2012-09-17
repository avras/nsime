
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

-include("nsime_icmpv4_protocol_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, set_node/2, protocol_number/1,
         recv/4, send_dest_unreach_frag_needed/4,
         send_time_exceeded_ttl/3, send_dest_unreach_port/3,
         set_ipv4_down_target/2, get_ipv4_down_target/1]).

-define(ICMPv4_PROTOCOL_NUMBER, 1).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(ProtocolPid) ->
    gen_server:call(ProtocolPid, terminate).

set_node(ProtocolPid, NodePid) ->
    gen_server:call(ProtocolPid, {set_node, NodePid}).

protocol_number(ProtocolPid) ->
    gen_server:call(ProtocolPid, protocol_number).

recv(ProtocolPid, Packet, Header, Interface) ->
    gen_server:call(ProtocolPid, {recv, Packet, Header, Interface}).

send(ProtocolPid, Packet, SrcAddress, DestAddress, SrcPort, DestPort, Route) ->
    gen_server:call(ProtocolPid, {send,
                                  Packet,
                                  SrcAddress,
                                  DestAddress,
                                  SrcPort,
                                  DestPort,
                                  Route
                                 }).

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

handle_call({recv, Packet, Header, Interface}, _From, ProtocolState) ->
    Data = Packet#nsime_packet.data,
    IcmpHeader = nsime_icmpv4_header:deserialize(Data),
    SrcAddress = nsime_ipv4_header:get_source_address(Header),
    DestAddress = nsime_ipv4_header:get_destination_address(Header),
    case IcmpHeader#nsime_icmpv4_header.type of
        ?ICMPv4_ECHO ->
            handle_echo(Packet, IcmpHeader, SrcAddress, DestAddress),
            {reply, rx_ok, ProtocolState};
        ?ICMPv4_DEST_UNREACH ->
            handle_dest_unreach(Packet, IcmpHeader, SrcAddress, DestAddress),
            {reply, rx_ok, ProtocolState};
        ?ICMPv4_TIME_EXCEEDED ->
            handle_time_exceeded(Packet, IcmpHeader, SrcAddress, DestAddress),
            {reply, rx_ok, ProtocolState};
        _ ->
            {reply, rx_ok, ProtocolState}
    end;

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

handle_echo(Packet, IcmpHeader, SrcAddress, DestAddress) ->
    <<EchoHeaderBinary:32, _Payload/binary>> = Packet#nsime_packet.data,
    ReplyPacket = #nsime_packet{data = EchoHeaderBinary, size = 4},
    send_message(ReplyPacket, DestAddress, SrcAddress, ?ICMPv4_ECHO_REPLY, 0, 0).
