%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Node module
%% Author : Saravanan Vijayakumaran

-module(nsime_node).
-author("Saravanan Vijayakumaran").

-include("nsime_node_state.hrl").

-export([create/0, create/1, destroy/1]).
-export([add_netdevice/2, get_netdevices/1, get_netdevice_count/1]).
-export([add_application/2, get_applications/1, get_application_count/1]).
-export([loop/1]).

create() ->
    NodeState = #nsime_node_state{},
    spawn(?MODULE, loop, [NodeState]).

create(NumNodes) ->
    case NumNodes of
        0 ->
            [];
        _ ->
            [spawn(?MODULE, loop, [#nsime_node_state{}]) | create(NumNodes-1)]
    end.

destroy(NodePid) ->
    Ref = erlang:monitor(process, NodePid),
    exit(NodePid, kill),
    receive
        {'DOWN', Ref, process, {NodePid, _Node}, Reason} ->
            Reason
    end.

add_netdevice(NodePid, DeviceType) ->
    Ref = make_ref(),
    NodePid ! {add_netdevice, self(), DeviceType, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_netdevices(NodePid) ->
    Ref = make_ref(),
    NodePid ! {get_netdevices, self(), Ref},
    receive
        {netdevices, DevicePids, Ref} ->
            DevicePids
    end.

get_netdevice_count(NodePid) ->
    Ref = make_ref(),
    NodePid ! {get_netdevice_count, self(), Ref},
    receive
        {netdevice_count, Length, Ref} ->
            Length
    end.

add_application(NodePid, ApplicationType) ->
    Ref = make_ref(),
    NodePid ! {add_application, self(), ApplicationType, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

get_applications(NodePid) ->
    Ref = make_ref(),
    NodePid ! {get_applications, self(), Ref},
    receive
        {applications, ApplicationPids, Ref} ->
            ApplicationPids
    end.

get_application_count(NodePid) ->
    Ref = make_ref(),
    NodePid ! {get_application_count, self(), Ref},
    receive
        {application_count, Length, Ref} ->
            Length
    end.

loop(State) ->
    receive
        {add_netdevice, From, DeviceType, Ref} ->
            DevicePid = DeviceType:create(),
            DeviceList = State#nsime_node_state.netdevices,
            NewState = State#nsime_node_state{netdevices = [DevicePid | DeviceList]},
            From ! {ok, Ref},
            loop(NewState);
        {get_netdevices, From, Ref} ->
            From ! {netdevices, State#nsime_node_state.netdevices, Ref},
            loop(State);
        {get_netdevice_count, From, Ref} ->
            From ! {netdevice_count, length(State#nsime_node_state.netdevices), Ref},
            loop(State);
        {add_application, From, ApplicationType, Ref} ->
            ApplicationPid = ApplicationType:create(),
            ApplicationList = State#nsime_node_state.applications,
            NewState = State#nsime_node_state{applications = [ApplicationPid | ApplicationList]},
            From ! {ok, Ref},
            loop(NewState);
        {get_applications, From, Ref} ->
            From ! {applications, State#nsime_node_state.applications, Ref},
            loop(State);
        {get_application_count, From, Ref} ->
            From ! {application_count, length(State#nsime_node_state.applications), Ref},
            loop(State)
    end.
