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

%% Purpose : IPv4 header module
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_header).
-author("Saravanan Vijayakumaran").

-include("nsime_ipv4_header.hrl").

-export([serialize/1, deserialize/1, enable_checksum/1,
         set_payload_size/2, get_payload_size/1,
         set_identification/2, get_identification/1,
         set_tos/2, get_tos/1, set_dscp/2, get_dscp/1,
         set_ecn/2, get_ecn/1, set_ttl/2, get_ttl/1,
         set_more_fragments/1, set_last_fragment/1,
         set_dont_fragment/1, set_may_fragment/1,
         is_last_fragment/1, is_dont_fragment/1,
         set_fragment_offset/2, get_fragment_offset/1,
         set_protocol/2, get_protocol/1, is_checksum_ok/1,
         set_source_address/2, get_source_address/1,
         set_destination_address/2, get_destination_address/1]).

-define(IP_VERSION, 4).
-define(DONT_FRAGMENT, 1).
-define(MORE_FRAGMENTS, 2).

serialize(Header) ->
    SrcAddress = binary:list_to_bin(
        tuple_to_list(
            Header#nsime_ipv4_header.source_address
        )
    ),
    DestAddress = binary:list_to_bin(
        tuple_to_list(
            Header#nsime_ipv4_header.destination_address
        )
    ),
    HeaderBinWithoutChecksum =
        <<
            ?IP_VERSION:4,
            (Header#nsime_ipv4_header.header_length):4,
            (Header#nsime_ipv4_header.tos):8,
            (Header#nsime_ipv4_header.total_length):16,
            (Header#nsime_ipv4_header.identification):16,
            (Header#nsime_ipv4_header.flags):3,
            (Header#nsime_ipv4_header.fragment_offset):13,
            (Header#nsime_ipv4_header.ttl):8,
            (Header#nsime_ipv4_header.protocol):8,
            0:16,
            SrcAddress:32,
            DestAddress:32
        >>,
    case Header#nsime_ipv4_header.calculate_checksum of
        false ->
            HeaderBinWithoutChecksum;
        true ->
            <<HeaderBeforeChecksum:80, 0:16, HeaderAfterChecksum:64>>
                = HeaderBinWithoutChecksum,
            Checksum = calculate_header_checksum(HeaderBinWithoutChecksum),
            <<HeaderBeforeChecksum:80, Checksum:16, HeaderAfterChecksum:64>>
    end.

deserialize(HeaderBinary) ->
    <<
        ?IP_VERSION:4, HL:4, TOS:8,
        TotalLength:16, Id:16, Flags:3, FragmentOffset:13,
        TTL:8, Protocol:8, HeaderChecksum:16,
        SrcAddress:32, DestAddress:32, _Rest/binary
    >> = HeaderBinary,
    #nsime_ipv4_header{
        header_length = HL,
        tos = TOS,
        total_length = TotalLength,
        identification = Id,
        flags = Flags,
        fragment_offset = FragmentOffset,
        ttl = TTL,
        protocol = Protocol,
        checksum = HeaderChecksum,
        source_address = list_to_tuple(binary:bin_to_list(SrcAddress)),
        destination_address = list_to_tuple(binary:bin_to_list(DestAddress)),
        checksum_correct = is_checksum_ok(HeaderBinary)
    }.

enable_checksum(Header) ->
    Header#nsime_ipv4_header{
        calculate_checksum = true
    }.

set_payload_size(Header, PayloadSize) ->
    HeaderLength = Header#nsime_ipv4_header.header_length,
    Header#nsime_ipv4_header{
        total_length = HeaderLength*4 + PayloadSize
    }.

get_payload_size(Header) ->
    Header#nsime_ipv4_header.total_length -
        4*Header#nsime_ipv4_header.header_length.

set_identification(Header, Id) ->
    Header#nsime_ipv4_header{
        identification = Id
    }.

get_identification(Header) ->
    Header#nsime_ipv4_header.identification.

set_tos(Header, TOS) ->
    Header#nsime_ipv4_header{
        tos = TOS
    }.

get_tos(Header) ->
    Header#nsime_ipv4_header.tos.

set_dscp(Header, DSCP) ->
    TOS = Header#nsime_ipv4_header.tos,
    <<NewTOS:8>> = <<DSCP:6, TOS:2>>,
    Header#nsime_ipv4_header{
        tos = NewTOS
    }.

get_dscp(Header) ->
    <<DSCP:6, _:2>> = <<(Header#nsime_ipv4_header.tos):8>>,
    DSCP.

set_ecn(Header, ECN) ->
    <<DSCP:6, _:2>> = <<(Header#nsime_ipv4_header.tos):8>>,
    <<NewTOS:8>> = <<DSCP:6, ECN:2>>,
    Header#nsime_ipv4_header{
        tos = NewTOS
    }.

get_ecn(Header) ->
    <<_:6, ECN:2>> = <<(Header#nsime_ipv4_header.tos):8>>,
    ECN.

set_ttl(Header, TTL) ->
    Header#nsime_ipv4_header{
        ttl = TTL
    }.

get_ttl(Header) ->
    Header#nsime_ipv4_header.ttl.

set_more_fragments(Header) ->
    Flags = Header#nsime_ipv4_header.flags,
    Header#nsime_ipv4_header{
        flags = Flags bor ?MORE_FRAGMENTS
    }.

set_last_fragment(Header) ->
    Flags = Header#nsime_ipv4_header.flags,
    <<Mask:3>> = <<bnot ?MORE_FRAGMENTS:3>>,
    Header#nsime_ipv4_header{
        flags = Flags band Mask
    }.

set_dont_fragment(Header) ->
    Flags = Header#nsime_ipv4_header.flags,
    Header#nsime_ipv4_header{
        flags = Flags bor ?DONT_FRAGMENT
    }.

set_may_fragment(Header) ->
    Flags = Header#nsime_ipv4_header.flags,
    <<Mask:3>> = <<bnot ?DONT_FRAGMENT:3>>,
    Header#nsime_ipv4_header{
        flags = Flags band Mask
    }.

is_last_fragment(Header) ->
    Flags = Header#nsime_ipv4_header.flags,
    (Flags band ?MORE_FRAGMENTS) == 0.

is_dont_fragment(Header) ->
    Flags = Header#nsime_ipv4_header.flags,
    (Flags band ?DONT_FRAGMENT) == 1.

set_fragment_offset(Header, FragmentOffset) ->
    Header#nsime_ipv4_header{
        fragment_offset = FragmentOffset
    }.

get_fragment_offset(Header) ->
    Header#nsime_ipv4_header.fragment_offset.

set_protocol(Header, Protocol) ->
    Header#nsime_ipv4_header{
        protocol = Protocol
    }.

get_protocol(Header) ->
    Header#nsime_ipv4_header.protocol.

is_checksum_ok(Header) ->
    Header#nsime_ipv4_header.checksum_correct.

set_source_address(Header, SrcAddress) ->
    Header#nsime_ipv4_header{
        source_address = SrcAddress
    }.

get_source_address(Header) ->
    Header#nsime_ipv4_header.source_address.

set_destination_address(Header, SrcAddress) ->
    Header#nsime_ipv4_header{
        destination_address = SrcAddress
    }.

get_destination_address(Header) ->
    Header#nsime_ipv4_header.destination_address.

%% Helper methods %%

calculate_header_checksum(HeaderBinary) ->
    <<A1:16, A2:16, A3:16, A4:16, A5:16>> = HeaderBinary,
    Sum = A1+A2+A3+A4+A5,
    <<Checksum:16>> = <<bnot((Sum band 65535) + (Sum bsr 16)):16>>,
    Checksum.
