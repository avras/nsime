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

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/0, create/1, create/2, destroy/1,
         set_local_address/2, get_local_address/1,
         set_broadcast_address/2, get_broadcast_address/1,
         set_mask/2, get_mask/1,
         set_address_scope/2, get_address_scope/1,
         is_secondary/1, set_secondary/1, set_primary/1]).

create() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

create(AddressState = #nsime_ipv4_interface_address_state{}) ->
    {ok, Pid} = gen_server:start(?MODULE, AddressState, []),
    Pid.

create(Address, Mask) ->
    {ok, Pid} = gen_server:start(?MODULE, {Address, Mask}, []),
    Pid.

destroy(AddressPid) ->
    gen_server:call(AddressPid, terminate).

set_local_address(AddressPid, Address) ->
    gen_server:call(AddressPid, {set_local_address, Address}).

get_local_address(AddressPid) ->
    gen_server:call(AddressPid, get_local_address).

set_broadcast_address(AddressPid, Address) ->
    gen_server:call(AddressPid, {set_broadcast_address, Address}).

get_broadcast_address(AddressPid) ->
    gen_server:call(AddressPid, get_broadcast_address).

set_mask(AddressPid, Mask) ->
    gen_server:call(AddressPid, {set_mask, Mask}).

get_mask(AddressPid) ->
    gen_server:call(AddressPid, get_mask).

set_address_scope(AddressPid, Scope) ->
    gen_server:call(AddressPid, {set_address_scope, Scope}).

get_address_scope(AddressPid) ->
    gen_server:call(AddressPid, get_address_scope).

is_secondary(AddressPid) ->
    gen_server:call(AddressPid, is_secondary).

set_secondary(AddressPid) ->
    gen_server:call(AddressPid, set_secondary).

set_primary(AddressPid) ->
    gen_server:call(AddressPid, set_primary).

init([]) ->
    AddressState = #nsime_ipv4_interface_address_state{},
    {ok, AddressState};

init(AddressState = #nsime_ipv4_interface_address_state{}) ->
    {ok, AddressState};

init({Address = {A1, A2, A3, A4}, Mask = {M1, M2, M3, M4}}) ->
    AddressState = #nsime_ipv4_interface_address_state{
        local_address = Address,
        mask = Mask,
        broadcast_address =
            {
                A1 bor (M1 bxor 255),
                A2 bor (M2 bxor 255),
                A3 bor (M3 bxor 255),
                A4 bor (M4 bxor 255)
            }
    },
    {ok, AddressState}.

handle_call({set_local_address, Address}, _From, AddressState) ->
    NewAddressState = AddressState#nsime_ipv4_interface_address_state{
        local_address = Address
    },
    {reply, ok, NewAddressState};

handle_call(get_local_address, _From, AddressState) ->
    Address = AddressState#nsime_ipv4_interface_address_state.local_address,
    {reply, Address, AddressState};

handle_call({set_broadcast_address, Address}, _From, AddressState) ->
    NewAddressState = AddressState#nsime_ipv4_interface_address_state{
        broadcast_address = Address
    },
    {reply, ok, NewAddressState};

handle_call(get_broadcast_address, _From, AddressState) ->
    Address = AddressState#nsime_ipv4_interface_address_state.broadcast_address,
    {reply, Address, AddressState};

handle_call({set_mask, Mask}, _From, AddressState) ->
    NewAddressState = AddressState#nsime_ipv4_interface_address_state{
        mask = Mask
    },
    {reply, ok, NewAddressState};

handle_call(get_mask, _From, AddressState) ->
    Mask = AddressState#nsime_ipv4_interface_address_state.mask,
    {reply, Mask, AddressState};

handle_call({set_address_scope, Scope}, _From, AddressState) ->
    NewAddressState = AddressState#nsime_ipv4_interface_address_state{
        address_scope = Scope
    },
    {reply, ok, NewAddressState};

handle_call(get_address_scope, _From, AddressState) ->
    Scope = AddressState#nsime_ipv4_interface_address_state.address_scope,
    {reply, Scope, AddressState};

handle_call(is_secondary, _From, AddressState) ->
    IsSecondary = AddressState#nsime_ipv4_interface_address_state.is_secondary,
    {reply, IsSecondary, AddressState};

handle_call(set_secondary, _From, AddressState) ->
    NewAddressState = AddressState#nsime_ipv4_interface_address_state{
        is_secondary = true
    },
    {reply, ok, NewAddressState};

handle_call(set_primary, _From, AddressState) ->
    NewAddressState = AddressState#nsime_ipv4_interface_address_state{
        is_secondary = false
    },
    {reply, ok, NewAddressState};

handle_call(terminate, _From, AddressState) ->
    {stop, normal, stopped, AddressState}.

handle_cast(_Request, AddressState) ->
    {noreply, AddressState}.

handle_info(_Request, AddressState) ->
    {noreply, AddressState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, AddressState, _Extra) ->
    {ok, AddressState}.
