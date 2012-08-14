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

-export([start/0, stop/0, add/1, delete/1, get_node_list/0]).
-export([loop/1]).

start() ->
    NodePidList = gb_sets:empty(),
    register(?MODULE, spawn(?MODULE, loop, [NodePidList])).

stop() ->
    Ref = erlang:monitor(process, ?MODULE),
    exit(whereis(?MODULE), kill),
    receive
        {'DOWN', Ref, process, {?MODULE, _Node}, Reason} ->
            Reason
    end.

add(NodePid) ->
    Ref = make_ref(),
    ?MODULE ! {add, self(), NodePid, Ref},
    receive
        {ok, Ref} ->
            ok
    end.

delete(NodePid) ->
    Ref = make_ref(),
    ?MODULE ! {delete, self(), NodePid, Ref},
    receive
        {ok, Ref} ->
            ok;
        {none, Ref} ->
            none
    end.

get_node_list() ->
    Ref = make_ref(),
    ?MODULE ! {get_node_list, self(), Ref},
    receive
        {ok, NodePidList, Ref} ->
            NodePidList
    end.

loop(NodePidList) ->
    receive
        {add, From, NodePid, Ref} -> 
            NewNodePidList = gb_sets:add(NodePid, NodePidList),
            From ! {ok, Ref},
            loop(NewNodePidList);
        {delete, From, NodePid, Ref} ->
            case gb_sets:is_element(NodePid, NodePidList) of
                true ->
                    NewNodePidList = gb_sets:delete(NodePid, NodePidList),
                    From ! {ok, Ref};
                false ->
                    NewNodePidList = NodePidList,
                    From ! {none, Ref}
                end,
            loop(NewNodePidList);
        {get_node_list, From, Ref} ->
            From ! {ok, NodePidList, Ref},
            loop(NodePidList)
    end.
