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

%% Purpose : IPv4 static routing protocol module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_static_routing).
-author("Saravanan Vijayakumaran").

-include("nsime_ipv4_static_routing_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, destroy/1, route_input/8, route_output/4,
         notify_interface_up/2, notify_interface_down/2, notify_add_address/3,
         notify_remove_address/3, set_ipv4_protocol/2, add_network_route/5,
         add_network_route/6, add_host_route/4, add_host_route/5,
         set_default_route/3, set_default_route/4, get_default_route/1,
         get_network_routes/1, get_route/2, remove_route/2]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

destroy(RoutingPid) ->
    gen_server:call(RoutingPid, terminate).

set_ipv4_protocol(RoutingPid, Ipv4ProtocolPid) ->
    gen_server:call(RoutingPid, {set_ipv4_protocol, Ipv4ProtocolPid}).

init([]) ->
    RoutingState = #nsime_ipv4_static_routing_state{},
    {ok, RoutingState};

handle_call({set_ipv4_protocol, Ipv4ProtocolPid}, _From, RoutingState) ->
    NewRoutingState = RoutingState#nsime_ipv4_static_routing_state{
        ipv4_protocol = Ipv4ProtocolPid
    },
    {reply, ok, NewRoutingState};

handle_call(terminate, _From, RoutingState) ->
    {stop, normal, stopped, RoutingState}.

handle_cast(_Request, RoutingState) ->
    {noreply, RoutingState}.

handle_info(_Request, RoutingState) ->
    {noreply, RoutingState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, RoutingState, _Extra) ->
    {ok, RoutingState}.
