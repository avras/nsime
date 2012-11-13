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

%% Purpose : Test module for nsime_layer4_protocol
%% Author : Saravanan Vijayakumaran

-module(nsime_layer4_protocol_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_types.hrl").
-include("nsime_packet.hrl").
-include("nsime_ipv4_header.hrl").
-include("nsime_ip_endpoint_state.hrl").
-include("nsime_ip_endpoint_demux_state.hrl").
-include("nsime_udp_protocol_state.hrl").
-include("nsime_ipv4_interface_address_state.hrl").
-include("nsime_ipv4_interface_state.hrl").

all() -> [
            test_protocol_number,
            test_recv
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_protocol_number(_) ->
    {ok, _Pid} = nsime_config:start(),
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    ?assertEqual(
        nsime_layer4_protocol:protocol_number(ProtocolPid),
        nsime_udp_protocol:protocol_number()
    ),
    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped),
    ?assertEqual(nsime_config:stop(), stopped).

test_recv(_) ->
    {ok, _Pid} = nsime_config:start(),
    ProtocolPid = nsime_udp_protocol:create(),
    ?assert(is_pid(ProtocolPid)),
    ?assertEqual(
        nsime_layer4_protocol:recv_icmp(ProtocolPid, junk, junk, junk, junk, junk, junk, junk, junk),
        ok
    ),

    Packet1 = #nsime_packet{data = <<1:160>>, size = 20},
    SrcAddress = {10, 107, 1, 1},
    DestAddress = {192, 168, 0, 1},
    Ipv4Header = #nsime_ipv4_header{
        source_address = SrcAddress,
        destination_address = DestAddress,
        ttl = 32
    },
    ?assertEqual(
        nsime_layer4_protocol:recv(ProtocolPid, Packet1, Ipv4Header, #nsime_ipv4_interface_state{}),
        rx_endpoint_unreach
    ),

    ?assertEqual(nsime_udp_protocol:destroy(ProtocolPid), stopped),
    ?assertEqual(nsime_config:stop(), stopped).
