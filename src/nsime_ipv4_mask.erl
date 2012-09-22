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

%% Purpose : IPv4 mask module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_mask).
-author("Saravanan Vijayakumaran").

-export([create/1, get_prefix_length/1, get_loopback/0,
         get_zero/0, get_ones/0, to_string/1]).

create(MaskString) ->
    case inet_parse:ipv4_address(MaskString) of
        {ok, Mask} ->
            Mask;
        {error, einval} ->
            erlang:error(invalid_argument)
    end.

get_prefix_length(Mask) ->
    AllOnes = <<16#FF, 16#FF, 16#FF, 16#FF>>,
    BinaryMask = list_to_binary(tuple_to_list(Mask)),
    Lengths = lists:seq(1, 32),
    lists:last(
        lists:takewhile(
            fun(N) ->
                RestSize = 32-N,
                <<OnesPrefix:N, _:RestSize>> = AllOnes,
                <<BinaryMaskPrefix:N, _:RestSize>> = BinaryMask,
                OnesPrefix == BinaryMaskPrefix
            end,
            Lengths
        )
    ).

get_loopback() ->
    {255, 0, 0, 0}.

get_zero() ->
    {0, 0, 0, 0}.

get_ones() ->
    {255, 255, 255, 255}.

to_string(Mask) ->
    inet_parse:ntoa(Mask).
