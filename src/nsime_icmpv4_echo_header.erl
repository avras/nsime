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

%% Purpose : ICMPv4 echo header module
%% Author : Saravanan Vijayakumaran

-module(nsime_icmpv4_echo_header).
-author("Saravanan Vijayakumaran").

-include("nsime_packet.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_icmpv4_headers.hrl").

-export([serialize/1, deserialize/1,
         set_identifier/2, get_identifier/1,
         set_sequence_number/2, get_sequence_number/1,
         set_data/2, get_data/1,
         set_data_size/2, get_data_size/1]).

serialize(Header) ->
    <<
        (Header#nsime_icmpv4_echo_header.identifier):16,
        (Header#nsime_icmpv4_echo_header.sequence_number):16,
        (Header#nsime_icmpv4_echo_header.data)/binary
    >>.

deserialize(HeaderBinary) ->
    <<Identifier:16, SequenceNumber:16, Data/binary>> = HeaderBinary,
    #nsime_icmpv4_echo_header{
        identifier = Identifier,
        sequence_number = SequenceNumber,
        data = Data,
        data_size = byte_size(Data)
    }.

set_identifier(Header, Identifier) ->
    Header#nsime_icmpv4_echo_header{identifier = Identifier}.

get_identifier(Header) ->
    Header#nsime_icmpv4_echo_header.identifier.

set_sequence_number(Header, SequenceNumber) ->
    Header#nsime_icmpv4_echo_header{sequence_number = SequenceNumber}.

get_sequence_number(Header) ->
    Header#nsime_icmpv4_echo_header.sequence_number.

set_data(Header, Packet) ->
    DataSize = Header#nsime_icmpv4_echo_header.data_size,
    <<Data:DataSize, _Rest/binary>> = Packet#nsime_packet.data,
    Header#nsime_icmpv4_echo_header{
        data = <<Data:DataSize>>
    }.

get_data(Header) ->
    Header#nsime_icmpv4_echo_header.data.

set_data_size(Header, DataSize) ->
    Header#nsime_icmpv4_echo_header{data_size = DataSize}.

get_data_size(Header) ->
    Header#nsime_icmpv4_echo_header.data_size.
