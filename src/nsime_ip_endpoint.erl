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

%% Purpose : IP endpoint module
%% Author : Saravanan Vijayakumaran

-module(nsime_ip_endpoint).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_event.hrl").
-include("nsime_ip_endpoint_state.hrl").

-export([create/0, create/3, get_id/1,
         get_local_address/1, set_local_address/2,
         get_local_port/1, set_local_port/2,
         set_peer/3, get_peer_address/1, get_peer_port/1,
         bind_to_netdevice/2, get_bound_netdevice/1,
         set_receive_callback/2, set_icmp_callback/2,
         set_destroy_callback/2, forward_up/5, forward_icmp/6]).

create() ->
    #nsime_ip_endpoint_state{
        endpoint_id = make_ref()
    }.

create(Address, Port, _Callbacks = {ReceiveCallback, IcmpCallback, DestroyCallback}) ->
    #nsime_ip_endpoint_state{
        endpoint_id = make_ref(),
        local_address = Address,
        local_port = Port,
        peer_address = nsime_ipv4_address:get_any(),
        peer_port = 0,
        receive_callback = ReceiveCallback,
        icmp_callback = IcmpCallback,
        destroy_callback = DestroyCallback
    }.

get_id(EndpointState) ->
    EndpointState#nsime_ip_endpoint_state.endpoint_id.

get_local_address(EndpointState) ->
    EndpointState#nsime_ip_endpoint_state.local_address.

set_local_address(EndpointState, Address) ->
    EndpointState#nsime_ip_endpoint_state{local_address = Address}.

get_local_port(EndpointState) ->
    EndpointState#nsime_ip_endpoint_state.local_port.

set_local_port(EndpointState, Port) ->
    EndpointState#nsime_ip_endpoint_state{local_port = Port}.

set_peer(EndpointState, Address, Port) ->
    EndpointState#nsime_ip_endpoint_state{
        peer_address = Address,
        peer_port = Port
    }.

get_peer_address(EndpointState) ->
    EndpointState#nsime_ip_endpoint_state.peer_address.

get_peer_port(EndpointState) ->
    EndpointState#nsime_ip_endpoint_state.peer_port.

bind_to_netdevice(EndpointState, DevicePid) ->
    EndpointState#nsime_ip_endpoint_state{
        bound_netdevice = DevicePid
    }.

get_bound_netdevice(EndpointState) ->
    EndpointState#nsime_ip_endpoint_state.bound_netdevice.

set_receive_callback(EndpointState, Callback) ->
    EndpointState#nsime_ip_endpoint_state{
        receive_callback = Callback
    }.

set_icmp_callback(EndpointState, Callback) ->
    EndpointState#nsime_ip_endpoint_state{
        icmp_callback = Callback
    }.

set_destroy_callback(EndpointState, Callback) ->
    EndpointState#nsime_ip_endpoint_state{
        destroy_callback = Callback
    }.

forward_up(EndpointState, Packet, Header, Port, Interface) ->
    nsime_callback:apply(
        EndpointState#nsime_ip_endpoint_state.receive_callback,
        [Packet, Header, Port, Interface]
    ).

forward_icmp(EndpointState, Source, TTL, Type, Code, Info) ->
    nsime_callback:apply(
        EndpointState#nsime_ip_endpoint_state.icmp_callback,
        [Source, TTL, Type, Code, Info]
    ).
