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
    gen_server:call(?MODULE, terminate, infinity).

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

terminate(_Reason, NodePidList) ->
    plist:pforeach(
        fun(Node) -> catch nsime_node:destroy(Node, infinity) end,
        gb_sets:to_list(NodePidList)
    ),
    ok.

code_change(_OldVersion, NodePidList, _Extra) ->
    {ok, NodePidList}.
