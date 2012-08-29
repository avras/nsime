%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Node list module
%% Author : Saravanan Vijayakumaran
%% Description: Implementation of singleton node list

-module(nsime_node_list).
-author("Saravanan Vijayakumaran").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0, add/1, delete/1, get_node_list/0]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, terminate).

add(NodePid) ->
    gen_server:call(?MODULE, {add, NodePid}).

delete(NodePid) ->
    gen_server:call(?MODULE, {delete, NodePid}).

get_node_list() ->
    gen_server:call(?MODULE, get_node_list).

init([]) ->
    NodePidList = gb_sets:empty(),
    {ok, NodePidList}.

handle_call({add, NodePid}, _From, NodePidList) ->
    NewNodePidList = gb_sets:add(NodePid, NodePidList),
    {reply, ok, NewNodePidList};

handle_call({delete, NodePid}, _From, NodePidList) ->
    case gb_sets:is_element(NodePid, NodePidList) of
        true ->
            NewNodePidList = gb_sets:delete(NodePid, NodePidList),
            {reply, ok, NewNodePidList};
        false ->
            {reply, none, NodePidList}
        end;

handle_call(get_node_list, _From, NodePidList) ->
    {reply, NodePidList, NodePidList};

handle_call(terminate, _From, NodePidList) ->
    {stop, normal, stopped, NodePidList}.

handle_cast(_Request, NodePidList) ->
    {noreply, NodePidList}.

handle_info(_Request, NodePidList) ->
    {noreply, NodePidList}.

terminate(_Reason, _NodePidList) ->
    ok.

code_change(_OldVersion, NodePidList, _Extra) ->
    {ok, NodePidList}.
