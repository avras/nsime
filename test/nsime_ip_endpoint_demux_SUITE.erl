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

%% Purpose : Test module for nsime_ip_endpoint_demux
%% Author : Saravanan Vijayakumaran

-module(nsime_ip_endpoint_demux_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_ip_endpoint_demux_state.hrl").

all() -> [
            test_creation_shutdown,
            test_endpoint_allocation,
            test_lookup,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    DemuxPid = nsime_ip_endpoint_demux:create(),
    ?assert(is_pid(DemuxPid)),
    ?assertEqual(nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid), []),
    ?assertEqual(nsime_ip_endpoint_demux:destroy(DemuxPid), stopped).

test_endpoint_allocation(_) ->
    DemuxPid = nsime_ip_endpoint_demux:create(),
    ?assert(is_pid(DemuxPid)),
    ?assertEqual(nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid), []),
    EndpointPid1 = nsime_ip_endpoint_demux:allocate(DemuxPid),
    EndpointList = nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid),
    ?assert([EndpointPid1] == EndpointList),
    ?assert(is_pid(EndpointPid1)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid1),
        nsime_ipv4_address:get_any()
    ),
    Port1 = nsime_ip_endpoint:get_local_port(EndpointPid1),
    ?assert(is_integer(Port1) and (Port1 =< 65535) and (Port1 >= 49152)),

    Address = {10, 107, 1, 1},
    EndpointPid2 = nsime_ip_endpoint_demux:allocate(DemuxPid, Address),
    ?assert(is_pid(EndpointPid2)),
    ?assert(
        lists:member(
            EndpointPid2,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid)
        )
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid2),
        Address
    ),
    Port2 = nsime_ip_endpoint:get_local_port(EndpointPid2),
    ?assert(is_integer(Port2) and (Port2 =< 65535) and (Port2 >= 49152)),

    ?assertError(
        duplicate_address_port,
        nsime_ip_endpoint_demux:allocate(
            DemuxPid,
            Address,
            Port2
        )
    ),

    Port3 = case Port2 + 1 == Port1 of
        true ->
            Port2 + 2;
        false ->
            Port2 + 1
    end,
    EndpointPid3 = nsime_ip_endpoint_demux:allocate(DemuxPid, Port3),
    ?assert(is_pid(EndpointPid3)),
    ?assert(
        lists:member(
            EndpointPid3,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid)
        )
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid3),
        nsime_ipv4_address:get_any()
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointPid3),
        Port3
    ),

    Address2 = {10, 107, 1, 2},
    Port4 = Port3 + 1,
    EndpointPid4 = nsime_ip_endpoint_demux:allocate(DemuxPid, Address2, Port4),
    ?assert(is_pid(EndpointPid4)),
    ?assert(
        lists:member(
            EndpointPid4,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid)
        )
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid4),
        Address2
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointPid4),
        Port4
    ),

    Address3 = {10, 107, 1, 3},
    Port5 = Port4 + 1,
    PeerAddress = {192, 168, 0, 1},
    PeerPort = 80,
    EndpointPid5 = nsime_ip_endpoint_demux:allocate(
        DemuxPid,
        Address3,
        Port5,
        PeerAddress,
        PeerPort
    ),
    ?assert(is_pid(EndpointPid5)),
    ?assert(
        lists:member(
            EndpointPid5,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid)
        )
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointPid5),
        Address3
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointPid5),
        Port5
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_address(EndpointPid5),
        PeerAddress
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_port(EndpointPid5),
        PeerPort
    ),
    ?assertError(
        duplicate_address_port,
        nsime_ip_endpoint_demux:allocate(
            DemuxPid,
            Address3,
            Port5,
            PeerAddress,
            PeerPort
        )
    ),

    ?assertEqual(nsime_ip_endpoint_demux:deallocate(DemuxPid, EndpointPid1), ok),
    ?assertNot(
        lists:member(
            EndpointPid1,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid)
        )
    ),
    ?assert(
        lists:foldl(
            fun(E, Acc) ->
                case Acc of
                    false ->
                        false;
                    true ->
                        lists:member(E,
                            nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid)
                        )
                end
            end,
            true,
            [EndpointPid2, EndpointPid3, EndpointPid4, EndpointPid5]
        )
    ),

    ?assertEqual(nsime_ip_endpoint_demux:destroy(DemuxPid), stopped).

test_lookup(_) ->
    DemuxPid = nsime_ip_endpoint_demux:create(),
    ?assert(is_pid(DemuxPid)),
    ?assertEqual(nsime_ip_endpoint_demux:get_all_endpoints(DemuxPid), []),
    EndpointPid1 = nsime_ip_endpoint_demux:allocate(DemuxPid),
    Port1 = nsime_ip_endpoint:get_local_port(EndpointPid1),
    ?assert(nsime_ip_endpoint_demux:lookup_port_local(DemuxPid, Port1)),
    ?assert(nsime_ip_endpoint_demux:lookup_local(DemuxPid, nsime_ipv4_address:get_any(), Port1)),
    ?assertNot(nsime_ip_endpoint_demux:lookup_local(DemuxPid, nsime_ipv4_address:get_any(), Port1+1)),
    ?assertNot(nsime_ip_endpoint_demux:lookup_local(DemuxPid, nsime_ipv4_address:get_broadcast(), Port1)),
    ?assertNot(nsime_ip_endpoint_demux:lookup_port_local(DemuxPid, Port1+1)),
    EndpointPid2 = nsime_ip_endpoint_demux:allocate(DemuxPid),
    nsime_ip_endpoint_demux:deallocate(DemuxPid, EndpointPid1),
    ?assertNot(nsime_ip_endpoint_demux:lookup_port_local(DemuxPid, Port1)),
    ?assertNot(nsime_ip_endpoint_demux:lookup_local(DemuxPid, nsime_ipv4_address:get_any(), Port1)),
    ?assertEqual(nsime_ip_endpoint_demux:destroy(DemuxPid), stopped).

test_cast_info_codechange(_) ->
    EndpointPid = nsime_ip_endpoint_demux:create(),
    ?assert(is_pid(EndpointPid)),
    gen_server:cast(EndpointPid, junk),
    EndpointPid ! junk,
    nsime_ip_endpoint_demux:code_change(junk, junk, junk),
    ?assertEqual(nsime_ip_endpoint_demux:destroy(EndpointPid), stopped).
