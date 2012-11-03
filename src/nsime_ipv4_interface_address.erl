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

%% Purpose : IPv4 interface address module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_interface_address).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").
-include("nsime_ipv4_interface_address_state.hrl").

-export([create/0, create/2,
         set_local_address/2, get_local_address/1,
         set_broadcast_address/2, get_broadcast_address/1,
         set_mask/2, get_mask/1,
         set_address_scope/2, get_address_scope/1,
         is_secondary/1, set_secondary/1, set_primary/1]).

create() ->
    #nsime_ipv4_interface_address_state{
        addressid = make_ref()
    }.

create(Address = {A1, A2, A3, A4}, Mask = {M1, M2, M3, M4}) ->
    #nsime_ipv4_interface_address_state{
        local_address = Address,
        mask = Mask,
        broadcast_address =
            {
                A1 bor (M1 bxor 255),
                A2 bor (M2 bxor 255),
                A3 bor (M3 bxor 255),
                A4 bor (M4 bxor 255)
            }
    }.

set_local_address(AddressState, Address) ->
    AddressState#nsime_ipv4_interface_address_state{
        local_address = Address
    }.

get_local_address(AddressState) ->
    AddressState#nsime_ipv4_interface_address_state.local_address.

set_broadcast_address(AddressState, Address) ->
    AddressState#nsime_ipv4_interface_address_state{
        broadcast_address = Address
    }.

get_broadcast_address(AddressState) ->
    AddressState#nsime_ipv4_interface_address_state.broadcast_address.

set_mask(AddressState, Mask) ->
    AddressState#nsime_ipv4_interface_address_state{
        mask = Mask
    }.

get_mask(AddressState) ->
    AddressState#nsime_ipv4_interface_address_state.mask.

set_address_scope(AddressState, Scope) ->
    AddressState#nsime_ipv4_interface_address_state{
        address_scope = Scope
    }.

get_address_scope(AddressState) ->
    AddressState#nsime_ipv4_interface_address_state.address_scope.

is_secondary(AddressState) ->
    AddressState#nsime_ipv4_interface_address_state.is_secondary.

set_secondary(AddressState) ->
    AddressState#nsime_ipv4_interface_address_state{
        is_secondary = true
    }.

set_primary(AddressState) ->
    AddressState#nsime_ipv4_interface_address_state{
        is_secondary = false
    }.
