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

%% Purpose : ICMPv4 time exceeded header module
%% Author : Saravanan Vijayakumaran

-module(nsime_icmpv4_time_exceeded_header).
-author("Saravanan Vijayakumaran").

-include("nsime_packet.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_icmpv4_headers.hrl").

-export([serialize/1, deserialize/1,
         set_header/2, get_header/1,
         set_data/2, get_data/1]).

serialize(Header) ->
    Ipv4Header = nsime_ipv4_header:serialize(
        Header#nsime_icmpv4_time_exceeded_header.header
    ),
    <<
        0:32,
        Ipv4Header/binary,
        (Header#nsime_icmpv4_time_exceeded_header.data):64
    >>.

deserialize(HeaderBinary) ->
    <<0:32, Ipv4HeaderBinary:160, Data:64, _Rest/binary>> = HeaderBinary,
    Ipv4Header = nsime_ipv4_header:deserialize(Ipv4HeaderBinary),
    #nsime_icmpv4_time_exceeded_header{
        header = Ipv4Header,
        data = <<Data:64>>
    }.

set_header(Header, Ipv4Header) ->
    Header#nsime_icmpv4_time_exceeded_header{header = Ipv4Header}.

get_header(Header) ->
    Header#nsime_icmpv4_time_exceeded_header.header.

set_data(Header, Packet) ->
    <<Data:64, _Rest/binary>> = Packet#nsime_packet.data,
    Header#nsime_icmpv4_time_exceeded_header{
        data = <<Data:64>>
    }.

get_data(Header) ->
    Header#nsime_icmpv4_time_exceeded_header.data.
