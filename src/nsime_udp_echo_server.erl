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

%% Purpose : UDP echo server
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_echo_server).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_packet.hrl").
-include("nsime_udp_echo_server_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, set_node/2, get_node/1,
         schedule_start/2, set_listen_port/2, start/1, stop/1, handle_read/1,
         get_transmit_trace_callback/1, set_transmit_trace_callback/2,
         get_receive_trace_callback/1, set_receive_trace_callback/2]).

create() ->
    {ok, ServerPid} = gen_server:start(?MODULE, [], []),
    ServerPid.

destroy(ServerPid) ->
    gen_server:call(ServerPid, terminate).

get_node(ServerPid) ->
    gen_server:call(ServerPid, get_node).

set_node(ServerPid, NodePid) ->
    gen_server:call(ServerPid, {set_node, NodePid}).

schedule_start(ServerPid, Time) ->
    gen_server:call(ServerPid, {schedule_start, Time}).

set_listen_port(ServerPid, Port) when
    is_integer(Port),
    Port >= 0,
    Port =< 65535
    ->
        gen_server:call(ServerPid, {set_listen_port, Port});
set_listen_port(_,_) ->
    erlang:error(invalid_argument).

start(ServerPid) ->
    gen_server:call(ServerPid, start).

stop(ServerPid) ->
    gen_server:call(ServerPid, stop).

handle_read(SocketPid) ->
    case nsime_udp_socket:recv_from(SocketPid) of
        {Packet = #nsime_packet{size = Size}, {Address, Port}} ->
            CurrentTime = nsime_simulator:current_time(),
            io:format(
                "At time ~p server received ~p bytes from " ++
                inet_parse:ntoa(Address) ++ " port ~p~n",
                [CurrentTime, Size, Port]
            ),
            nsime_udp_socket:send_to(SocketPid, Packet, 0, {Address, Port}),
            io:format(
                "At time ~p server sent ~p bytes to " ++ inet_parse:ntoa(Address) ++ " port ~p~n",
                [CurrentTime, Size, Port]
            ),
            handle_read(SocketPid);
        none ->
            ok
    end.

get_transmit_trace_callback(ServerPid) ->
    gen_server:call(ServerPid, get_transmit_trace_callback).

set_transmit_trace_callback(ServerPid, Callback) ->
    gen_server:call(ServerPid, {set_transmit_trace_callback, Callback}).

get_receive_trace_callback(ServerPid) ->
    gen_server:call(ServerPid, get_receive_trace_callback).

set_receive_trace_callback(ServerPid, Callback) ->
    gen_server:call(ServerPid, {set_receive_trace_callback, Callback}).

init([]) ->
    ServerState = #nsime_udp_echo_server_state{},
    {ok, ServerState}.

handle_call(get_node, _From, ServerState) ->
    NodePid = ServerState#nsime_udp_echo_server_state.node,
    {reply, NodePid, ServerState};

handle_call({set_node, NodePid}, _From, ServerState) ->
    NewServerState = ServerState#nsime_udp_echo_server_state{node = NodePid},
    {reply, ok, NewServerState};

handle_call({schedule_start, Time}, _From, ServerState) ->
    StartEvent = #nsime_event{
        module = ?MODULE,
        function = start,
        arguments = [self()],
        eventid = make_ref()
    },
    nsime_simulator:schedule(Time, StartEvent),
    {reply, ok, ServerState};

handle_call({set_listen_port, Port}, _From, ServerState) ->
    NewServerState = ServerState#nsime_udp_echo_server_state{listen_port = Port},
    {reply, ok, NewServerState};

handle_call(start, _From, ServerState) ->
    SocketPid = ServerState#nsime_udp_echo_server_state.socket,
    Port = ServerState#nsime_udp_echo_server_state.listen_port,
    case SocketPid of
        undefined ->
            NodePid = ServerState#nsime_udp_echo_server_state.node,
            UdpProtocolPid = nsime_node:get_object(NodePid, nsime_udp_protocol),
            NewSocket = nsime_udp_protocol:create_socket(UdpProtocolPid),
            nsime_udp_socket:bind(NewSocket, {nsime_ipv4_address:get_any(), Port}),
            nsime_udp_socket:set_receive_callback(NewSocket, {nsime_udp_echo_server, handle_read, [NewSocket]}),
            NewServerState = ServerState#nsime_udp_echo_server_state{
                socket = NewSocket
            },
            {reply, ok, NewServerState};
        _ ->
            nsime_udp_socket:set_receive_callback(SocketPid, {nsime_udp_echo_server, handle_read, [SocketPid]}),
            {reply, ok, ServerState}
    end;

handle_call(stop, _From, ServerState) ->
    SocketPid = ServerState#nsime_udp_echo_server_state.socket,
    if is_pid(SocketPid) ->
        nsime_udp_socket:close(SocketPid),
        nsime_udp_socket:set_receive_callback(SocketPid, {none, none, []}),
        NewServerState = ServerState#nsime_udp_echo_server_state{
            socket = undefined
        }
    end,
    {reply, ok, NewServerState};

handle_call(get_transmit_trace_callback, _From, ServerState) ->
    Callback = ServerState#nsime_udp_echo_server_state.transmit_trace_callback,
    {reply, Callback, ServerState};

handle_call({set_transmit_trace_callback, Callback}, _From, ServerState) ->
    NewServerState = ServerState#nsime_udp_echo_server_state{transmit_trace_callback = Callback},
    {reply, ok, NewServerState};

handle_call(get_receive_trace_callback, _From, ServerState) ->
    Callback = ServerState#nsime_udp_echo_server_state.receive_trace_callback,
    {reply, Callback, ServerState};

handle_call({set_receive_trace_callback, Callback}, _From, ServerState) ->
    NewServerState = ServerState#nsime_udp_echo_server_state{receive_trace_callback = Callback},
    {reply, ok, NewServerState};

handle_call(terminate, _From, ServerState) ->
    {stop, normal, stopped, ServerState}.

handle_cast(_Request, ServerState) ->
    {noreply, ServerState}.

handle_info(_Request, ServerState) ->
    {noreply, ServerState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, ServerState, _Extra) ->
    {ok, ServerState}.
