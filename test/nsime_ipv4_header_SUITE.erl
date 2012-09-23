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

%% Purpose : Test module for nsime_ipv4_header
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_header_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include("nsime_ipv4_header.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [
          {group, testgroup_all}
         ].

groups() ->
    [{
        testgroup_all,
        [parallel],
        [
          test_serialization,
          test_set_get_components,
          test_properties
        ]
    }].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(testgroup_all, Config) ->
    Config.

end_per_group(testgroup_all, Config) ->
    Config.

test_serialization(_) ->
    HL = 5,
    TOS = 200,
    TotalLength = 60,
    Id = 12345,
    Flags = 5,
    FragmentOffset = 5000,
    TTL = 32,
    Protocol = 100,
    SrcAddress = {10, 107, 1, 2},
    DestAddress = {192, 168, 0, 1},
    Header = #nsime_ipv4_header{
        header_length = HL,
        tos = TOS,
        total_length = TotalLength,
        identification = Id,
        flags = Flags,
        fragment_offset = FragmentOffset,
        ttl = TTL,
        protocol = Protocol,
        checksum = 0,
        source_address = SrcAddress,
        destination_address = DestAddress,
        calculate_checksum = false,
        checksum_correct = false
    },
    ?assertEqual(
        nsime_ipv4_header:deserialize(
            nsime_ipv4_header:serialize(Header)
        ),
        Header#nsime_ipv4_header{calculate_checksum = true}
    ),
    HeaderBinary =
        <<
            ?IP_VERSION:4, HL:4, TOS:8,
            TotalLength:16, Id:16, Flags:3, FragmentOffset:13,
            TTL:8, Protocol:8, 0:16,
            (binary:list_to_bin(tuple_to_list(SrcAddress)))/binary,
            (binary:list_to_bin(tuple_to_list(DestAddress)))/binary
        >>,
    Checksum = calculate_header_checksum(HeaderBinary),
    HeaderWithChecksum = Header#nsime_ipv4_header{
        checksum = Checksum,
        calculate_checksum = true,
        checksum_correct = true
    },
    HeaderBinWithChecksum = nsime_ipv4_header:serialize(HeaderWithChecksum),
    ?assertEqual(
        nsime_ipv4_header:deserialize(HeaderBinWithChecksum),
        HeaderWithChecksum
    ).

test_set_get_components(_) ->
    Header = #nsime_ipv4_header{},
    ?assert(Header#nsime_ipv4_header.calculate_checksum),
    ?assertNot(nsime_ipv4_header:is_checksum_ok(Header)),
    Header1 = nsime_ipv4_header:enable_checksum(Header),
    ?assert(Header1#nsime_ipv4_header.calculate_checksum),
    PayloadSize = 10,
    Header2 = nsime_ipv4_header:set_payload_size(Header, PayloadSize),
    ?assertEqual(nsime_ipv4_header:get_payload_size(Header2), PayloadSize),
    Id = 12345,
    Header3 = nsime_ipv4_header:set_identification(Header, Id),
    ?assertEqual(nsime_ipv4_header:get_identification(Header3), Id),
    TOS = 200,
    Header4 = nsime_ipv4_header:set_tos(Header, TOS),
    ?assertEqual(nsime_ipv4_header:get_tos(Header4), TOS),
    DSCP = 63,
    Header5 = nsime_ipv4_header:set_dscp(Header, DSCP),
    ?assertEqual(nsime_ipv4_header:get_dscp(Header5), DSCP),
    ECN = 3,
    Header6 = nsime_ipv4_header:set_ecn(Header, ECN),
    ?assertEqual(nsime_ipv4_header:get_ecn(Header6), ECN),
    TTL = 32,
    Header7 = nsime_ipv4_header:set_ttl(Header, TTL),
    ?assertEqual(nsime_ipv4_header:get_ttl(Header7), TTL),
    FragmentOffset = 5000,
    Header8 = nsime_ipv4_header:set_fragment_offset(Header, FragmentOffset),
    ?assertEqual(nsime_ipv4_header:get_fragment_offset(Header8), FragmentOffset),
    Protocol = 100,
    Header9 = nsime_ipv4_header:set_protocol(Header, Protocol),
    ?assertEqual(nsime_ipv4_header:get_protocol(Header9), Protocol),
    SrcAddress = {10, 107, 1, 2},
    Header10 = nsime_ipv4_header:set_source_address(Header, SrcAddress),
    ?assertEqual(nsime_ipv4_header:get_source_address(Header10), SrcAddress),
    DestAddress = {192, 168, 0, 1},
    Header11 = nsime_ipv4_header:set_destination_address(Header, DestAddress),
    ?assertEqual(nsime_ipv4_header:get_destination_address(Header11), DestAddress).

test_properties(_) ->
    Header = #nsime_ipv4_header{},
    Header1 = nsime_ipv4_header:set_more_fragments(Header),
    ?assertNot(nsime_ipv4_header:is_last_fragment(Header1)),
    Header2 = nsime_ipv4_header:set_last_fragment(Header1),
    ?assert(nsime_ipv4_header:is_last_fragment(Header2)),
    Header3 = nsime_ipv4_header:set_dont_fragment(Header2),
    ?assert(nsime_ipv4_header:is_dont_fragment(Header3)),
    Header4 = nsime_ipv4_header:set_may_fragment(Header3),
    ?assertNot(nsime_ipv4_header:is_dont_fragment(Header4)).

%% Helper methods %%

calculate_header_checksum(HeaderBinary) ->
    <<A1:16, A2:16, A3:16, A4:16, A5:16, A6:16, A7:16, A8:16, A9:16, A10:16>>
        = HeaderBinary,
    Sum = A1+A2+A3+A4+A5+A6+A7+A8+A9+A10,
    <<Checksum:16>> = <<bnot((Sum band 65535) + (Sum bsr 16)):16>>,
    Checksum.
