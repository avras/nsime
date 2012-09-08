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

%% Purpose : Netdevice module
%% Author : Saravanan Vijayakumaran

-module(nsime_netdevice).
-author("Saravanan Vijayakumaran").

-export([get_device_type/1,
         set_interface_index/2, get_interface_index/1,
         set_address/2, get_address/1,
         get_channel/1, set_mtu/2, get_mtu/1,
         is_link_up/1, is_bridge/1, is_ptp/1,
         is_broadcast/1, get_broadcast_address/1,
         is_multicast/1, get_multicast_address/2,
         set_node/2, get_node/1,
         needs_arp/1, supports_send_from/1]).

get_device_type(DevicePid) ->
    gen_server:call(DevicePid, get_device_type).

set_interface_index(DevicePid, DeviceIndex) ->
    gen_server:call(DevicePid, {set_device_index, DeviceIndex}).

get_interface_index(DevicePid) ->
    gen_server:call(DevicePid, get_interface_index).

set_address(DevicePid, Address) ->
    gen_server:call(DevicePid, {set_address, Address}).

get_address(DevicePid) ->
    gen_server:call(DevicePid, get_address).

get_channel(DevicePid) ->
    gen_server:call(DevicePid, get_channel).

set_mtu(DevicePid, MTU) ->
    gen_server:call(DevicePid, {set_mtu, MTU}).

get_mtu(DevicePid) ->
    gen_server:call(DevicePid, get_mtu).

is_link_up(DevicePid) ->
    gen_server:call(DevicePid, is_link_up).

is_bridge(DevicePid) ->
    gen_server:call(DevicePid, is_bridge).

is_ptp(DevicePid) ->
    gen_server:call(DevicePid, is_ptp).

is_broadcast(DevicePid) ->
    gen_server:call(DevicePid, is_broadcast).

get_broadcast_address(DevicePid) ->
    gen_server:call(DevicePid, get_broadcast_address).

is_multicast(DevicePid) ->
    gen_server:call(DevicePid, is_multicast).

get_multicast_address(DevicePid, MulticastGroupAddress) ->
    gen_server:call(DevicePid, {get_multicast_address, MulticastGroupAddress}).

set_node(DevicePid, NodePid) ->
    gen_server:call(DevicePid, {set_node, NodePid}).

get_node(DevicePid) ->
    gen_server:call(DevicePid, get_node).

needs_arp(DevicePid) ->
    gen_server:call(DevicePid, needs_arp).

supports_send_from(DevicePid) ->
    gen_server:call(DevicePid, supports_send_from).
