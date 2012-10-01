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

%% Purpose : Test module for nsime_udp_header
%% Author : Saravanan Vijayakumaran

-module(nsime_udp_header_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include("nsime_packet.hrl").
-include("nsime_udp_header.hrl").
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
          test_checksum
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
    SrcPort = 8080,
    DestPort = 80,
    Length = 1000,
    Checksum = 0,
    Header = #nsime_udp_header{
        source_port = SrcPort,
        destination_port = DestPort,
        length = Length,
        checksum = Checksum
    },
    ?assertEqual(
        nsime_udp_header:deserialize(
            nsime_udp_header:serialize(
                SrcPort,
                DestPort,
                Length,
                Checksum
            )
        ),
        Header
    ),
    ?assertEqual(
        nsime_udp_header:deserialize(
            nsime_udp_header:serialize(Header)
        ),
        Header
    ).

test_checksum(_) ->
    B1 = <<0:32>>,
    ?assertEqual(nsime_udp_header:calculate_checksum(B1), 16#FFFF),
    B2 = <<0:40>>,
    ?assertEqual(nsime_udp_header:calculate_checksum(B2), 16#FFFF),
    Packet1 = #nsime_packet{data = <<0:160>>, size = 20},
    ?assertError(
        ipv6_not_supported,
        nsime_udp_header:calculate_checksum(
            Packet1,
            {0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0},
            0,
            0,
            0,
            0
        )
    ),
    ?assertError(
        invalid_argument,
        nsime_udp_header:calculate_checksum(
            Packet1,
            {0, 0},
            {0, 0},
            0,
            0,
            0,
            0
        )
    ),
    ?assertEqual(
        nsime_udp_header:calculate_checksum(
            Packet1,
            {0, 0, 0, 0},
            {0, 0, 0, 0},
            0,
            0,
            0,
            0
        ),
        16#FFFF
    ),
    Packet2 = Packet1#nsime_packet{
        tags = [
                {source_address, {0, 0, 0, 0}},
                {destination_address, {0, 0, 0, 0}}
               ]
    },
    ?assertEqual(
        nsime_udp_header:calculate_checksum(Packet2)
        bor
        nsime_udp_protocol:protocol_number(),
        16#FFFF
    ),
    ok.

