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

-export([create/0, destroy/1,
         set_node/2, get_node/1,
         schedule_start/2, set_listen_port/2,
         start/1, stop/1, handle_read/1]).

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
    StartEvent = #nsime_event{
        module = ?MODULE,
        function = start,
        arguments = [ServerPid],
        eventid = make_ref()
    },
    nsime_simulator:schedule(Time, StartEvent).

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
    case nsime_udp_socket:receive_from(SocketPid) of
        {Packet = #nsime_packet{size = Size}, Address} ->
            CurrentTime = nsime_simulator:current_time(),
            io:format("At time ~p server received ~p bytes from ~p",
                [CurrentTime, Size, inet_parse:ntoa(Address)]
            ),
            nsime_udp_socket:send_to(Packet, 0, Address),
            io:format("At time ~p server received ~p bytes from ~p",
                [CurrentTime, Size, inet_parse:ntoa(Address)]
            ),
            handle_read(SocketPid);
        {none, _} ->
            ok
    end.

init([]) ->
    ServerState = #nsime_udp_echo_server_state{},
    {ok, ServerState}.

handle_call(get_node, _From, ServerState) ->
    NodePid = ServerState#nsime_udp_echo_server_state.node,
    {reply, NodePid, ServerState};

handle_call({set_node, NodePid}, _From, ServerState) ->
    NewServerState = ServerState#nsime_udp_echo_server_state{node = NodePid},
    {reply, ok, NewServerState};

handle_call({set_listen_port, Port}, _From, ServerState) ->
    NewServerState = ServerState#nsime_udp_echo_server_state{listen_port = Port},
    {reply, ok, NewServerState};

handle_call(start, _From, ServerState) ->
    SocketPid = ServerState#nsime_udp_echo_server_state.socket,
    Port = ServerState#nsime_udp_echo_server_state.listen_port,
    case SocketPid of
        undefined ->
            NewSocket = nsime_udp_socket:create(),
            {ok, Address} = inet_parse:address("0.0.0.0"),
            nsime_udp_socket:bind(NewSocket, Address, Port),
            nsime_udp_socket:set_recv_callback(NewSocket, ?MODULE, handle_read, [NewSocket]),
            NewServerState = ServerState#nsime_udp_echo_server_state{
                socket = NewSocket
            },
            {reply, ok, NewServerState};
        _ ->
            nsime_udp_socket:set_recv_callback(SocketPid, ?MODULE, handle_read, [SocketPid]),
            {reply, ok, ServerState}
    end;

handle_call(stop, _From, ServerState) ->
    SocketPid = ServerState#nsime_udp_echo_server_state.socket,
    if is_pid(SocketPid) ->
        nsime_udp_socket:close(SocketPid),
        nsime_udp_socket:set_recv_callback(SocketPid, none, none, []),
        NewServerState = ServerState#nsime_udp_echo_server_state{
            socket = undefined
        }
    end,
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
