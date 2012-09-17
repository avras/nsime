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

%% Purpose : ICMPv4 header module
%% Author : Saravanan Vijayakumaran

-module(nsime_icmpv4_header).
-author("Saravanan Vijayakumaran").

-include("nsime_ipv4_header.hrl").
-include("nsime_icmpv4_headers.hrl").

-export([serialize/1, deserialize/1, enable_checksum/1,
         set_type/2, get_type/1, set_code/2, get_code/1]).

serialize(Header) ->
    HeaderBinWithoutChecksum =
        <<
            (Header#nsime_icmpv4_header.type):8,
            (Header#nsime_icmpv4_header.code):8,
            0:16
        >>,
    case Header#nsime_icmpv4_header.calculate_checksum of
        false ->
            HeaderBinWithoutChecksum;
        true ->
            <<TypeAndCode:16, 0:16>> = HeaderBinWithoutChecksum,
            <<Checksum:16>> = <<bnot(TypeAndCode):16>>,
            <<TypeAndCode:16, Checksum:16>>
    end.

deserialize(HeaderBinary) ->
    <<Type:8, Code:8, _Rest/binary>> = HeaderBinary,
    #nsime_icmpv4_header{
        type = Type,
        code = Code
    }.

enable_checksum(Header) ->
    Header#nsime_icmpv4_header{calculate_checksum = true}.

set_type(Header, Type) ->
    Header#nsime_icmpv4_header{type = Type}.

get_type(Header) ->
    Header#nsime_icmpv4_header.type.

set_code(Header, Code) ->
    Header#nsime_icmpv4_header{code = Code}.

get_code(Header) ->
    Header#nsime_icmpv4_header.code.
