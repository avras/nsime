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

%% Purpose : IPv4 interface module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_interface).
-author("Saravanan Vijayakumaran").

-include("nsime_ipv4_interface_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, set_node/2, 
         set_device/2, get_device/1,
         set_arp_cache/2, get_arp_cache/1,
         set_metric/2, get_metric/1,
         is_up/1, is_down/1, set_up/1, set_down/1,
         is_forwarding/1, set_forwarding/2,
         send/3, add_address/2, remove_address/2,
         get_address/2, get_num_addresses/1]). 

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(InterfacePid) ->
    gen_server:call(InterfacePid, terminate).

init([]) ->
    InterfaceState = #nsime_ipv4_interface_state{},
    {ok, InterfaceState}.

handle_call(terminate, _From, InterfaceState) ->
    {stop, normal, stopped, InterfaceState}.

handle_cast(_Request, InterfaceState) ->
    {noreply, InterfaceState}.

handle_info(_Request, InterfaceState) ->
    {noreply, InterfaceState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, InterfaceState, _Extra) ->
    {ok, InterfaceState}.
