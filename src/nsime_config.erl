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

%% Purpose : Configuration store module
%% Author : Saravanan Vijayakumaran
%% Description: Implementation of singleton configuration store

-module(nsime_config).
-author("Saravanan Vijayakumaran").

-include("nsime_config_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0, enable_checksum/0, disable_checksum/0,
         checksum_enabled/0]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, terminate).

init([]) ->
    ConfigState = #nsime_config_state{},
    {ok, ConfigState}.

enable_checksum() ->
    gen_server:call(?MODULE, enable_checksum).

disable_checksum() ->
    gen_server:call(?MODULE, disable_checksum).

checksum_enabled() ->
    gen_server:call(?MODULE, checksum_enabled).

handle_call(enable_checksum, _From, ConfigState) ->
    Store = ConfigState#nsime_config_state.store,
    NewConfigState = ConfigState#nsime_config_state{
        store = [{checksum_enabled, true} |
                 proplists:delete(checksum_enabled, Store)]
    },
    {reply, ok, NewConfigState};

handle_call(disable_checksum, _From, ConfigState) ->
    Store = ConfigState#nsime_config_state.store,
    NewConfigState = ConfigState#nsime_config_state{
        store = [{checksum_enabled, false} |
                 proplists:delete(checksum_enabled, Store)]
    },
    {reply, ok, NewConfigState};

handle_call(checksum_enabled, _From, ConfigState) ->
    ChecksumEnabled = proplists:get_bool(
        checksum_enabled,
        ConfigState#nsime_config_state.store
    ),
    {reply, ChecksumEnabled, ConfigState};

handle_call(terminate, _From, ConfigState) ->
    {stop, normal, stopped, ConfigState}.

handle_cast(_Request, ConfigState) ->
    {noreply, ConfigState}.

handle_info(_Request, ConfigState) ->
    {noreply, ConfigState}.

terminate(_Reason, _ConfigState) ->
    ok.

code_change(_OldVersion, ConfigState, _Extra) ->
    {ok, ConfigState}.
