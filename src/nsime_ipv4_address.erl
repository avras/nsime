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

%% Purpose : IPv4 address module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_address).
-author("Saravanan Vijayakumaran").

-export([serialize/1, deserialize/1, is_broadcast/1, is_multicast/1, 
         is_local_multicast/1, combine_mask/2, get_subnet_directed_broadcast/2,
         is_subnet_directed_broadcast/2, get_zero/0, get_any/0, get_broadcast/0
         get_loopback/0, to_string/1]).

serialize(Address) ->
    list_to_binary(tuple_to_list(Address)).

deserialize(<<A1:8, A2:8, A3:8, A4:8>>) ->
    {A1, A2, A3, A4}.

is_broadcast(Address) ->
    Address == {255, 255, 255, 255}.

is_multicast({A1, A2, A3, A4}) ->
    (A1 >= 224) and (A1 =< 239) and
    (A2 >= 0) and (A2 =< 255) and
    (A3 >= 0) and (A3 =< 255) and
    (A4 >= 0) and (A4 =< 255).

is_local_multicast({A1, A2, A3, _A4}) ->
    (A1 == 224) and (A2 == 0) and (A3 == 0).

combine_mask({A1, A2, A3, A4}, {M1, M2, M3, M4}) ->
    {A1 band M1, A2 band M2, A3 band M3, A4 and M4}.

get_subnet_directed_broadcast(
    {A1, A2, A3, A4}, 
    Mask = {M1, M2, M3, M4}
) ->
    case Mask == nsime_ipv4_mask:get_ones() of
        false ->
            {
                A1 bor (M1 bxor 255),
                A2 bor (M2 bxor 255),
                A3 bor (M3 bxor 255),
                A4 bor (M4 bxor 255)
            };
        true ->
            erlang:error(invalid_argument)
    end.

is_subnet_directed_broadcast(
    Address = {A1, A2, A3, A4}, 
    Mask = {M1, M2, M3, M4}
) ->
    case Mask == nsime_ipv4_mask:get_ones() of
        false ->
            Address == {
                        A1 bor (M1 bxor 255),
                        A2 bor (M2 bxor 255),
                        A3 bor (M3 bxor 255),
                        A4 bor (M4 bxor 255)
                       };
        true ->
            erlang:error(invalid_argument)
    end.
    
get_zero() ->
    {0, 0, 0, 0}.

get_any() ->
    {0, 0, 0, 0}.

get_broadcast() ->
    {255, 255, 255, 255}.

get_loopback() ->
    {127, 0, 0, 1}.

to_string(Address) ->
    inet_parse:ntoa(Address).
