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

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, create/1, destroy/1, add_netdevice/2,
         get_netdevices/1, get_netdevice_count/1,
         add_application/2, get_applications/1,
         get_application_count/1]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

create(NumNodes) ->
    case NumNodes of
        0 ->
            [];
        _ ->
            [create() | create(NumNodes-1)]
    end.

destroy(NodePid) ->
    gen_server:call(NodePid, terminate).

add_netdevice(NodePid, DeviceType) ->
    gen_server:call(NodePid, {add_netdevice, DeviceType}).

get_netdevices(NodePid) ->
    gen_server:call(NodePid, get_netdevices).

get_netdevice_count(NodePid) ->
    gen_server:call(NodePid, get_netdevice_count).

add_application(NodePid, ApplicationType) ->
    gen_server:call(NodePid, {add_application, ApplicationType}).

get_applications(NodePid) ->
    gen_server:call(NodePid, get_applications).

get_application_count(NodePid) ->
    gen_server:call(NodePid, get_application_count).

init([]) ->
    State = #nsime_node_state{},
    {ok, State}.

handle_call({add_netdevice, DeviceType}, _From, State) ->
    DevicePid = DeviceType:create(),
    DeviceList = State#nsime_node_state.netdevices,
    NewState = State#nsime_node_state{netdevices = [DevicePid | DeviceList]},
    {reply, ok, NewState};

handle_call(get_netdevices, _From, State) ->
    {reply, State#nsime_node_state.netdevices, State};

handle_call(get_netdevice_count, _From, State) ->
    {reply, length(State#nsime_node_state.netdevices), State};

handle_call({add_application, ApplicationType}, _From, State) ->
    ApplicationPid = ApplicationType:create(),
    ApplicationList = State#nsime_node_state.applications,
    NewState = State#nsime_node_state{applications = [ApplicationPid | ApplicationList]},
    {reply, ok, NewState};

handle_call(get_applications, _From, State) ->
    {reply, State#nsime_node_state.applications, State};

handle_call(get_application_count, _From, State) ->
    {reply, length(State#nsime_node_state.applications), State};

handle_call(terminate, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
