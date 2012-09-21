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

%% Purpose : Node module
%% Author : Saravanan Vijayakumaran

-module(nsime_node).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_node_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, create/1, destroy/1, add_object/3, get_object/2,
         add_netdevice/2, get_netdevices/1, add_application/2, get_applications/1,
         register_protocol_handler/5, unregister_protocol_handler/2,
         receive_from_device/8, promisc_receive_from_device/7,
         nonpromisc_receive_from_device/5]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

create(NumNodes) ->
    case NumNodes of
        0 ->
            [];
        _ ->
            lists:map(fun(_) -> create() end, lists:seq(1, NumNodes))
    end.

destroy(NodePid) ->
    gen_server:call(NodePid, terminate).

add_object(NodePid, ObjectName, ObjectPid) ->
    gen_server:call(NodePid, {add_object, ObjectName, ObjectPid}).

get_object(NodePid, ObjectName) ->
    gen_server:call(NodePid, {get_object, ObjectName}).

add_netdevice(NodePid, DevicePid) ->
    gen_server:call(NodePid, {add_netdevice, DevicePid}).

get_netdevices(NodePid) ->
    gen_server:call(NodePid, get_netdevices).

add_application(NodePid, ApplicationPid) ->
    gen_server:call(NodePid, {add_application, ApplicationPid}).

get_applications(NodePid) ->
    gen_server:call(NodePid, get_applications).

register_protocol_handler(NodePid, ProtocolHandler, Protocol, DevicePid, Promiscuous) ->
    gen_server:call(NodePid, {register_protocol_handler,
                                  ProtocolHandler,
                                  Protocol,
                                  DevicePid,
                                  Promiscuous
                             }
    ).

unregister_protocol_handler(NodePid, ProtocolHandler) ->
    gen_server:call(NodePid, {unregister_protocol_handler, ProtocolHandler}).

receive_from_device(NodePid, DevicePid, Packet, Protocol, FromAddress, ToAddress, PacketType, Promiscuous) ->
    gen_server:call(NodePid, {receive_from_device,
                                  DevicePid,
                                  Packet,
                                  Protocol,
                                  FromAddress,
                                  ToAddress,
                                  PacketType,
                                  Promiscuous
                             }
    ).

promisc_receive_from_device(NodePid, DevicePid, Packet, Protocol, FromAddress, ToAddress, PacketType) ->
    receive_from_device(NodePid, DevicePid, Packet, Protocol, FromAddress, ToAddress, PacketType, true).

nonpromisc_receive_from_device(NodePid, DevicePid, Packet, Protocol, FromAddress) ->
    ToAddress = nsime_netdevice:get_address(DevicePid),
    receive_from_device(NodePid, DevicePid, Packet, Protocol, FromAddress, ToAddress, undefined, false).

init([]) ->
    NodeState = #nsime_node_state{},
    {ok, NodeState}.

handle_call({add_object, ObjectName, ObjectPid}, _From, NodeState) ->
    ObjectList = NodeState#nsime_node_state.objects,
    NewObjectList = [{ObjectName, ObjectPid} | ObjectList],
    NewNodeState = NodeState#nsime_node_state{objects = NewObjectList},
    {reply, ok, NewNodeState};

handle_call({get_object, ObjectName}, _From, NodeState) ->
    ObjectList = NodeState#nsime_node_state.objects,
    ObjectPid = proplists:get_value(ObjectName, ObjectList),
    {reply, ObjectPid, NodeState};

handle_call({add_netdevice, DevicePid}, _From, NodeState) ->
    nsime_netdevice:set_node(DevicePid, self()),
    nsime_netdevice:set_receive_callback(
        DevicePid,
        {
            ?MODULE,
            nonpromisc_receive_from_device,
            [self()]
        }
    ),
    DeviceList = NodeState#nsime_node_state.netdevices,
    NewNodeState = NodeState#nsime_node_state{netdevices = [DevicePid | DeviceList]},
    {reply, ok, NewNodeState};

handle_call(get_netdevices, _From, NodeState) ->
    {reply, NodeState#nsime_node_state.netdevices, NodeState};

handle_call({add_application, ApplicationPid}, _From, NodeState) ->
    nsime_application:set_node(ApplicationPid, self()),
    nsime_application:schedule_start(ApplicationPid, {0, sec}),
    ApplicationList = NodeState#nsime_node_state.applications,
    NewNodeState = NodeState#nsime_node_state{applications = [ApplicationPid | ApplicationList]},
    {reply, ok, NewNodeState};

handle_call(get_applications, _From, NodeState) ->
    {reply, NodeState#nsime_node_state.applications, NodeState};

handle_call({register_protocol_handler,
                ProtocolHandler,
                Protocol,
                DevicePid,
                Promiscuous
            },
            _From,
            NodeState
) ->
    ProtocolHandlerRecord = #nsime_protocol_handler_record{
        handler = ProtocolHandler,
        device = DevicePid,
        protocol = Protocol,
        promiscuous = Promiscuous
    },
    ProtocolHandlerList = #nsime_node_state.protocol_handlers,
    NewProtocolHandlerList = [ProtocolHandlerRecord | ProtocolHandlerList],
    NewNodeState = NodeState#nsime_node_state{protocol_handlers = NewProtocolHandlerList},
    case Promiscuous of
        true ->
            case is_pid(DevicePid) of
                true ->
                    nsime_netdevice:set_promisc_receive_callback(
                        DevicePid,
                        {
                            ?MODULE,
                            promisc_receive_from_device,
                            [self()]
                        }
                    ),
                    {reply, ok, NewNodeState};
                false ->
                    DeviceList = NodeState#nsime_node_state.netdevices,
                    lists:foreach(
                        fun(D) ->
                            nsime_netdevice:set_promisc_receive_callback(
                                D,
                                {
                                    ?MODULE,
                                    promisc_receive_from_device,
                                    [self()]
                                }
                            )
                        end,
                        DeviceList
                    ),
                    {reply, ok, NewNodeState}
            end;
        false ->
            {reply, ok, NewNodeState}
    end;

handle_call({unregister_protocol_handler, ProtocolHandler}, _From, NodeState) ->
    ProtocolHandlerList = #nsime_node_state.protocol_handlers,
    NewProtocolHandlerList = lists:filter(
        fun(P) ->
            P#nsime_protocol_handler_record.handler =/= ProtocolHandler
        end,
        ProtocolHandlerList
    ),
    NewNodeState = NodeState#nsime_node_state{protocol_handlers = NewProtocolHandlerList},
    {reply, ok, NewNodeState};

handle_call({receive_from_device,
                DevicePid,
                Packet,
                Protocol,
                FromAddress,
                ToAddress,
                PacketType,
                Promiscuous
            },
            _From,
            NodeState
) ->
    ProtocolHandlerList = #nsime_node_state.protocol_handlers,
    Found = lists:foldl(
        fun(P, Match) ->
            HandlerDevice = P#nsime_protocol_handler_record.device,
            case
                (is_pid(HandlerDevice) == false) bor
                (HandlerDevice == DevicePid)
            of
                true ->
                    HandlerProtocol = P#nsime_protocol_handler_record.protocol,
                    case
                        (HandlerProtocol == 0) bor
                        (HandlerProtocol == Protocol)
                    of
                        true ->
                            case P#nsime_protocol_handler_record.promiscuous == Promiscuous of
                                true ->
                                    {Mod, Fun, Args} = P#nsime_protocol_handler_record.handler,
                                    NewArgs = lists:flatten([Args | [DevicePid, Packet, Protocol, FromAddress, ToAddress, PacketType]]),
                                    erlang:apply(Mod, Fun, NewArgs),
                                    true;
                                false ->
                                    Match
                            end;
                        false ->
                            Match
                    end;
                false ->
                    Match
            end
        end,
        false,
        ProtocolHandlerList
    ),
    {reply, Found, NodeState};

handle_call(terminate, _From, NodeState) ->
    DeviceList = NodeState#nsime_node_state.netdevices,
    lists:foreach(
        fun(D) ->
            nsime_netdevice:destroy(D)
        end,
        DeviceList
    ),
    ApplicationList = NodeState#nsime_node_state.applications,
    lists:foreach(
        fun(A) ->
            nsime_netdevice:destroy(A)
        end,
        ApplicationList
    ),
    {stop, normal, stopped, NodeState}.

handle_cast(_Request, NodeState) ->
    {noreply, NodeState}.

handle_info(_Request, NodeState) ->
    {noreply, NodeState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, NodeState, _Extra) ->
    {ok, NodeState}.
