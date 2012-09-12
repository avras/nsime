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

%% Purpose : UDP header module
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_header).
-author("Saravanan Vijayakumaran").

-include("nsime_packet.hrl").
-include("nsime_udp_header.hrl").

-export([serialize/1, deserialize/1,
         calculate_header_checksum/7]).

serialize(Header) ->
        <<
            (Header#nsime_udp_header.source_port):16,
            (Header#nsime_udp_header.destination_port):16,
            (Header#nsime_udp_header.length):16,
            (Header#nsime_udp_header.checksum):16
        >>.

deserialize(HeaderBinary) ->
    <<SourcePort:16, DestinationPort:16, Length:16, Checksum:16>>
        = HeaderBinary,
    #nsime_udp_header{
        source_port = SourcePort,
        destination_port = DestinationPort,
        length = Length,
        checksum = Checksum
    }.

calculate_header_checksum(
    Packet,
    SourceAddress,
    DestinationAddress,
    SourcePort,
    DestinationPort,
    Length,
    Protocol
) ->
    case {tuple_size(SourceAddress), tuple_size(DestinationAddress)} of
        {4, 4} ->
            S = binary:list_to_bin(tuple_to_list(SourceAddress)),
            D = binary:list_to_bin(tuple_to_list(DestinationAddress)),
            Data = Packet#nsime_packet.data,
            Sum = halfword_sum(
                <<
                    S/binary, D/binary, 0:8, Protocol:8, Length:16,
                    SourcePort:16, DestinationPort:16, Length:16,
                    Data/binary
                >>
            ),
            <<Checksum:16>> = <<bnot((Sum band 65535) + (Sum bsr 16)):16>>,
            Checksum;
        {6, 6} ->
            erlang:error(ipv6_not_supported);
        {_, _} ->
            erlang:error(invalid_argument)
    end.

%% Helper methods %%

halfword_sum(Binary) ->
    halfword_sum(Binary, 0).

halfword_sum(Binary, Sum) ->
    case Binary of
        <<Halfword:16, Rest/binary>> ->
            halfword_sum(Rest, Sum + Halfword);
        <<Byte:8>> ->
            halfword_sum(<<>>, Sum + Byte);
        <<>> ->
            Sum
    end.
