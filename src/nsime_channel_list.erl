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

%% Purpose : Channel list module
%% Author : Saravanan Vijayakumaran
%% Description: Implementation of singleton channel list

-module(nsime_channel_list).
-author("Saravanan Vijayakumaran").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0, add/1, delete/1, get_channel_list/0]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, terminate, infinity).

add(ChannelPid) ->
    gen_server:call(?MODULE, {add, ChannelPid}).

delete(ChannelPid) ->
    gen_server:call(?MODULE, {delete, ChannelPid}).

get_channel_list() ->
    gen_server:call(?MODULE, get_channel_list).

init([]) ->
    ChannelPidList = gb_sets:empty(),
    {ok, ChannelPidList}.

handle_call({add, ChannelPid}, _From, ChannelPidList) ->
    NewChannelPidList = gb_sets:add(ChannelPid, ChannelPidList),
    {reply, ok, NewChannelPidList};

handle_call({delete, ChannelPid}, _From, ChannelPidList) ->
    case gb_sets:is_element(ChannelPid, ChannelPidList) of
        true ->
            NewChannelPidList = gb_sets:delete(ChannelPid, ChannelPidList),
            {reply, ok, NewChannelPidList};
        false ->
            {reply, none, ChannelPidList}
        end;

handle_call(get_channel_list, _From, ChannelPidList) ->
    {reply, ChannelPidList, ChannelPidList};

handle_call(terminate, _From, ChannelPidList) ->
    {stop, normal, stopped, ChannelPidList}.

handle_cast(_Request, ChannelPidList) ->
    {noreply, ChannelPidList}.

handle_info(_Request, ChannelPidList) ->
    {noreply, ChannelPidList}.

terminate(_Reason, ChannelPidList) ->
    plists:foreach(
        fun(Channel) -> catch gen_server:call(Channel, terminate, infinity) end,
        gb_sets:to_list(ChannelPidList),
        {processes, 4}
    ),
    ok.

code_change(_OldVersion, ChannelPidList, _Extra) ->
    {ok, ChannelPidList}.
