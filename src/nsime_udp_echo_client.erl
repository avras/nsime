%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : UDP echo client
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_echo_client).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_packet.hrl").
-include("nsime_udp_echo_client_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, 
         set_node/2, get_node/1, 
         schedule_start/2, set_remote/3,
         set_data_size/2, get_data_size/1,
         start/1, stop/1, send/1, handle_read/1]).

create() ->
    {ok, ClientPid} = gen_server:start(?MODULE, [], []),
    ClientPid.

destroy(ClientPid) ->
    gen_server:call(ClientPid, terminate).

get_node(ClientPid) ->
    gen_server:call(ClientPid, get_node).

set_node(ClientPid, NodePid) ->
    gen_server:call(ClientPid, {set_node, NodePid}).

schedule_start(ClientPid, Time) ->
    gen_server:call(ClientPid, {schedule_start, Time}).

set_remote(ClientPid, Address, Port) when 
    is_list(Address), 
    is_integer(Port),
    Port >= 0,
    Port =< 65535
    ->
        gen_server:call(ClientPid, {set_remote, Address, Port});
set_remote(_,_,_) ->
    erlang:error(invalid_argument).

get_data_size(ClientPid) ->
    gen_server:call(ClientPid, get_data_size).

set_data_size(ClientPid, DataSize) ->
    gen_server:call(ClientPid, {set_data_size, DataSize}).

start(ClientPid) ->
    gen_server:call(ClientPid, start).

stop(ClientPid) ->
    gen_server:call(ClientPid, stop).

send(ClientPid) ->
    gen_server:call(ClientPid, send).

handle_read(SocketPid) ->
    case nsime_udp_socket:receive_from(SocketPid) of
        {#nsime_packet{size = Size}, Address} -> 
            CurrentTime = nsime_simulator:current_time(),
            io:format("At time ~p client received ~p bytes from ~p",
                [CurrentTime, Size, inet_parse:ntoa(Address)] 
            ),
            handle_read(SocketPid);
        {none, _} ->
            ok
    end.

init([]) ->
    ClientState = #nsime_udp_echo_client_state{},
    {ok, ClientState}.

handle_call(get_node, _From, ClientState) ->
    NodePid = ClientState#nsime_udp_echo_client_state.node,
    {reply, NodePid, ClientState};

handle_call({set_node, NodePid}, _From, ClientState) ->
    NewClientState = ClientState#nsime_udp_echo_client_state{node = NodePid},
    {reply, ok, NewClientState};

handle_call({schedule_start, Time}, _From, ClientState) ->
    StartEvent = #nsime_event{
        module = ?MODULE,
        function = start,
        arguments = [self()],
        eventid = make_ref()
    },
    nsime_simulator:schedule(Time, StartEvent),
    {reply, ok, ClientState};

handle_call({set_remote, Address, Port}, _From, ClientState) ->
    case inet_parse:address(Address) of
        {ok, AddressTuple} ->
            NewClientState = ClientState#nsime_udp_echo_client_state{
                peer_address = AddressTuple,
                peer_port = Port
            },
            {reply, ok, NewClientState};
        {error, _} ->
            erlang:error(invalid_argument)
    end;

handle_call(get_data_size, _From, ClientState) ->
    DataSize = ClientState#nsime_udp_echo_client_state.data_size,
    {reply, DataSize, ClientState};

handle_call({set_data_size, DataSize}, _From, ClientState) ->
    NewClientState = ClientState#nsime_udp_echo_client_state{data_size = DataSize},
    {reply, ok, NewClientState};

handle_call(start, _From, ClientState) ->
    SendEvent = #nsime_event{
        module = ?MODULE,
        function = send,
        arguments = [self()],
        eventid = make_ref()
    },
    SocketPid = ClientState#nsime_udp_echo_client_state.socket,
    case SocketPid of
        undefined ->
            NewSocket = nsime_udp_socket:create(),
            nsime_udp_socket:bind(NewSocket),
            Address = ClientState#nsime_udp_echo_client_state.peer_address,
            Port = ClientState#nsime_udp_echo_client_state.peer_port,
            nsime_udp_socket:connect(NewSocket, Address, Port),
            nsime_udp_socket:set_recv_callback(NewSocket, ?MODULE, handle_read, [NewSocket]),
            NewClientState = ClientState#nsime_udp_echo_client_state{
                socket = NewSocket,
                send_event = SendEvent
            };
        _ ->
            nsime_udp_socket:set_recv_callback(SocketPid, ?MODULE, handle_read, [SocketPid]),
            NewClientState = ClientState#nsime_udp_echo_client_state{
                send_event = SendEvent
            }
    end,
    nsime_simulator:schedule_now(SendEvent),
    {reply, ok, NewClientState};

handle_call(stop, _From, ClientState) ->
    SocketPid = ClientState#nsime_udp_echo_client_state.socket,
    SendEvent = ClientState#nsime_udp_echo_client_state.send_event,
    if is_pid(SocketPid) ->
        nsime_udp_socket:close(SocketPid),
        nsime_udp_socket:set_recv_callback(SocketPid, none, none, []),
        NewClientState = ClientState#nsime_udp_echo_client_state{
            socket = undefined,
            send_event = undefined
        }
    end,
    nsime_simulator:cancel(SendEvent),
    {reply, ok, NewClientState};

handle_call(send, _From, ClientState) ->
    Data = ClientState#nsime_udp_echo_client_state.data,
    DataSize = ClientState#nsime_udp_echo_client_state.data_size,
    SocketPid = ClientState#nsime_udp_echo_client_state.socket,
    case Data of
        undefined ->
            Packet = #nsime_packet{
                size = DataSize,
                data = <<0:(DataSize*8)>>,
                id = make_ref()
            };
        _ ->
            Packet = #nsime_packet{
                size = DataSize,
                data = Data,
                id = make_ref()
            } 
    end,
    erlang:apply(
        ClientState#nsime_udp_echo_client_state.transmit_trace_callback,
        [Packet]
    ),
    nsime_udp_socket:send(SocketPid, Packet),
    NumSentPackets = ClientState#nsime_udp_echo_client_state.num_sent_packets + 1,
    if NumSentPackets < ClientState#nsime_udp_echo_client_state.max_packets ->
        SendEvent = #nsime_event{
            module = ?MODULE,
            function = send,
            arguments = [self()],
            eventid = make_ref()
        },
        nsime_simulator:schedule(
        ClientState#nsime_udp_echo_client_state.inter_packet_gap,
        SendEvent
        )
    end,
    NewClientState = ClientState#nsime_udp_echo_client_state{num_sent_packets = NumSentPackets},
    {reply, ok, NewClientState};

handle_call(terminate, _From, ClientState) ->
    {stop, normal, stopped, ClientState}.

handle_cast(_Request, ClientState) ->
    {noreply, ClientState}.

handle_info(_Request, ClientState) ->
    {noreply, ClientState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, ClientState, _Extra) ->
    {ok, ClientState}.
