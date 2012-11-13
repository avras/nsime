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

-include("nsime_types.hrl").
-include("nsime_ip_endpoint_state.hrl").
-include("nsime_ip_endpoint_demux_state.hrl").

all() -> [
            test_creation_shutdown,
            test_endpoint_allocation,
            test_simple_lookup,
            test_lookup
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_creation_shutdown(_) ->
    DemuxState = nsime_ip_endpoint_demux:create(),
    ?assert(is_record(DemuxState, nsime_ip_endpoint_demux_state)),
    ?assertEqual(nsime_ip_endpoint_demux:get_all_endpoints(DemuxState), []).

test_endpoint_allocation(_) ->
    UdpSocketPid = nsime_udp_socket:create(),
    Callbacks = create_endpoint_callbacks(UdpSocketPid),
    DemuxState = nsime_ip_endpoint_demux:create(),
    ?assert(is_record(DemuxState, nsime_ip_endpoint_demux_state)),
    ?assertEqual(nsime_ip_endpoint_demux:get_all_endpoints(DemuxState), []),
    {EndpointState1, DemuxState1} = nsime_ip_endpoint_demux:allocate(DemuxState, Callbacks),
    EndpointList = nsime_ip_endpoint_demux:get_all_endpoints(DemuxState1),
    ?assert([EndpointState1] == EndpointList),
    ?assert(is_record(EndpointState1, nsime_ip_endpoint_state)),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState1),
        nsime_ipv4_address:get_any()
    ),
    Port1 = nsime_ip_endpoint:get_local_port(EndpointState1),
    ?assert(is_integer(Port1) and (Port1 =< 65535) and (Port1 >= 49152)),

    Address = {10, 107, 1, 1},
    {EndpointState2, DemuxState2} = nsime_ip_endpoint_demux:allocate(DemuxState1, Address, Callbacks),
    ?assert(is_record(EndpointState2, nsime_ip_endpoint_state)),
    ?assert(
        lists:member(
            EndpointState2,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxState2)
        )
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState2),
        Address
    ),
    Port2 = nsime_ip_endpoint:get_local_port(EndpointState2),
    ?assert(is_integer(Port2) and (Port2 =< 65535) and (Port2 >= 49152)),

    ?assertError(
        duplicate_address_port,
        nsime_ip_endpoint_demux:allocate(
            DemuxState2,
            Address,
            Port2,
            Callbacks
        )
    ),

    Port3 = case Port2 + 1 == Port1 of
        true ->
            Port2 + 2;
        false ->
            Port2 + 1
    end,
    {EndpointState3, DemuxState3} = nsime_ip_endpoint_demux:allocate(DemuxState2, Port3, Callbacks),
    ?assert(is_record(EndpointState3, nsime_ip_endpoint_state)),
    ?assert(
        lists:member(
            EndpointState3,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxState3)
        )
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState3),
        nsime_ipv4_address:get_any()
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointState3),
        Port3
    ),

    Address2 = {10, 107, 1, 2},
    Port4 = Port3 + 1,
    {EndpointState4, DemuxState4} = nsime_ip_endpoint_demux:allocate(DemuxState3, Address2, Port4, Callbacks),
    ?assert(is_record(EndpointState4, nsime_ip_endpoint_state)),
    ?assert(
        lists:member(
            EndpointState4,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxState4)
        )
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState4),
        Address2
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointState4),
        Port4
    ),

    Address3 = {10, 107, 1, 3},
    Port5 = Port4 + 1,
    PeerAddress = {192, 168, 0, 1},
    PeerPort = 80,
    {EndpointState5, DemuxState5} = nsime_ip_endpoint_demux:allocate(
        DemuxState4,
        Address3,
        Port5,
        PeerAddress,
        PeerPort,
        Callbacks
    ),
    ?assert(is_record(EndpointState5, nsime_ip_endpoint_state)),
    ?assert(
        lists:member(
            EndpointState5,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxState5)
        )
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_address(EndpointState5),
        Address3
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_local_port(EndpointState5),
        Port5
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_address(EndpointState5),
        PeerAddress
    ),
    ?assertEqual(
        nsime_ip_endpoint:get_peer_port(EndpointState5),
        PeerPort
    ),
    ?assertError(
        duplicate_address_port,
        nsime_ip_endpoint_demux:allocate(
            DemuxState5,
            Address3,
            Port5,
            PeerAddress,
            PeerPort,
            Callbacks
        )
    ),

    DemuxState6 = nsime_ip_endpoint_demux:deallocate(DemuxState5, EndpointState1),
    ?assertNot(
        lists:member(
            EndpointState1,
            nsime_ip_endpoint_demux:get_all_endpoints(DemuxState6)
        )
    ),
    ?assert(
        lists:foldl(
            fun(E, Acc) ->
                case Acc of
                    false ->
                        false;
                    true ->
                        lists:member(
                            E,
                            nsime_ip_endpoint_demux:get_all_endpoints(DemuxState6)
                        )
                end
            end,
            true,
            [EndpointState2, EndpointState3, EndpointState4, EndpointState5]
        )
    ).

test_simple_lookup(_) ->
    UdpSocketPid = nsime_udp_socket:create(),
    Callbacks = create_endpoint_callbacks(UdpSocketPid),
    DemuxState = nsime_ip_endpoint_demux:create(),
    ?assert(is_record(DemuxState, nsime_ip_endpoint_demux_state)),
    ?assertEqual(nsime_ip_endpoint_demux:get_all_endpoints(DemuxState), []),
    {EndpointState1, DemuxState1} = nsime_ip_endpoint_demux:allocate(DemuxState, Callbacks),
    Port1 = nsime_ip_endpoint:get_local_port(EndpointState1),
    ?assert(nsime_ip_endpoint_demux:lookup_port_local(DemuxState1, Port1)),
    ?assert(nsime_ip_endpoint_demux:lookup_local(DemuxState1, nsime_ipv4_address:get_any(), Port1)),
    ?assertNot(nsime_ip_endpoint_demux:lookup_local(DemuxState1, nsime_ipv4_address:get_any(), Port1+1)),
    ?assertNot(nsime_ip_endpoint_demux:lookup_local(DemuxState1, nsime_ipv4_address:get_broadcast(), Port1)),
    ?assertNot(nsime_ip_endpoint_demux:lookup_port_local(DemuxState1, Port1+1)),
    DemuxState2 = nsime_ip_endpoint_demux:deallocate(DemuxState1, EndpointState1),
    ?assertNot(nsime_ip_endpoint_demux:lookup_port_local(DemuxState2, Port1)),
    ?assertNot(nsime_ip_endpoint_demux:lookup_local(DemuxState2, nsime_ipv4_address:get_any(), Port1)),

    LocalAddress = {10, 107, 1, 3},
    LocalPort = 8080,
    PeerAddress = {192, 168, 0, 1},
    PeerPort = 80,
    {EndpointState2, DemuxState3} = nsime_ip_endpoint_demux:allocate(
        DemuxState2,
        LocalAddress,
        LocalPort,
        PeerAddress,
        PeerPort,
        Callbacks
    ),
    {EndpointState3, DemuxState4} = nsime_ip_endpoint_demux:allocate(DemuxState3, LocalPort, Callbacks),
    ?assertEqual(
        nsime_ip_endpoint_demux:simple_lookup(
            DemuxState4,
            LocalAddress,
            LocalPort,
            PeerAddress,
            PeerPort
        ),
        EndpointState2
    ),
    ?assertError(
        ipv6_not_supported,
        nsime_ip_endpoint_demux:simple_lookup(
            DemuxState4,
            {0, 0, 0, 0, 0, 0, 0, 0},
            undefined,
            {0, 0, 0, 0, 0, 0, 0, 0},
            undefined
        )
    ),
    ?assertError(
        invalid_argument,
        nsime_ip_endpoint_demux:simple_lookup(
            DemuxState4,
            {0, 0},
            undefined,
            {0, 0},
            undefined
        )
    ),
    ?assertEqual(nsime_ip_endpoint:get_local_port(EndpointState2), LocalPort),
    ?assertEqual(nsime_ip_endpoint:get_local_port(EndpointState3), LocalPort),
    ?assertEqual(
        nsime_ip_endpoint_demux:simple_lookup(
            DemuxState4,
            LocalAddress,
            LocalPort,
            PeerAddress,
            PeerPort + 1
        ),
        EndpointState2
    ),
    DemuxState5 = nsime_ip_endpoint_demux:deallocate(DemuxState4, EndpointState2),
    ?assertEqual(
        nsime_ip_endpoint_demux:simple_lookup(
            DemuxState5,
            LocalAddress,
            LocalPort,
            PeerAddress,
            PeerPort + 1
        ),
        EndpointState3
    ),
    {EndpointState4, DemuxState6} = nsime_ip_endpoint_demux:allocate(
        DemuxState5,
        nsime_ipv4_address:get_any(),
        LocalPort,
        nsime_ipv4_address:get_any(),
        PeerPort,
        Callbacks
    ),
    ?assertEqual(
        nsime_ip_endpoint_demux:simple_lookup(
            DemuxState6,
            LocalAddress,
            LocalPort,
            PeerAddress,
            PeerPort
        ),
        EndpointState4
    ),
    DemuxState7 = nsime_ip_endpoint_demux:deallocate(DemuxState6, EndpointState4),
    {EndpointState5, DemuxState8} = nsime_ip_endpoint_demux:allocate(
        DemuxState7,
        LocalAddress,
        LocalPort,
        nsime_ipv4_address:get_any(),
        PeerPort,
        Callbacks
    ),
    ?assertEqual(
        nsime_ip_endpoint_demux:simple_lookup(
            DemuxState8,
            LocalAddress,
            LocalPort,
            PeerAddress,
            PeerPort
        ),
        EndpointState5
    ),
    DemuxState9 = nsime_ip_endpoint_demux:deallocate(DemuxState8, EndpointState5),
    {EndpointState6, DemuxState10} = nsime_ip_endpoint_demux:allocate(
        DemuxState9,
        nsime_ipv4_address:get_any(),
        LocalPort,
        PeerAddress,
        PeerPort,
        Callbacks
    ),
    ?assertEqual(
        nsime_ip_endpoint_demux:simple_lookup(
            DemuxState10,
            LocalAddress,
            LocalPort,
            PeerAddress,
            PeerPort
        ),
        EndpointState6
    ).

test_lookup(_) ->
    UdpSocketPid = nsime_udp_socket:create(),
    Callbacks = create_endpoint_callbacks(UdpSocketPid),
    DemuxState = nsime_ip_endpoint_demux:create(),
    LocalAddress = {10, 107, 1, 3},
    LocalPort = 8080,
    PeerAddress = {192, 168, 0, 1},
    PeerPort = 80,
    {EndpointState1, DemuxState1} = nsime_ip_endpoint_demux:allocate(
        DemuxState,
        LocalAddress,
        LocalPort,
        PeerAddress,
        PeerPort,
        Callbacks
    ),
    {EndpointState2, DemuxState2} = nsime_ip_endpoint_demux:allocate(DemuxState1, LocalPort, Callbacks),
    ?assertError(
        ipv6_not_supported,
        nsime_ip_endpoint_demux:lookup(
            DemuxState2,
            {0, 0, 0, 0, 0, 0, 0, 0},
            undefined,
            {0, 0, 0, 0, 0, 0, 0, 0},
            undefined,
            undefined
        )
    ),
    ?assertError(
        invalid_argument,
        nsime_ip_endpoint_demux:lookup(
            DemuxState2,
            {0, 0},
            undefined,
            {0, 0},
            undefined,
            undefined
        )
    ).

test_port_allocation_failure(_) ->
    UdpSocketPid = nsime_udp_socket:create(),
    Callbacks = create_endpoint_callbacks(UdpSocketPid),
    DemuxState = #nsime_ip_endpoint_demux_state{},
    FirstPort = DemuxState#nsime_ip_endpoint_demux_state.first_port,
    LastPort = DemuxState#nsime_ip_endpoint_demux_state.last_port,
    DemuxState = nsime_ip_endpoint_demux:create(),
    parallel_allocate(DemuxState, FirstPort, LastPort, Callbacks),
    ?assertError(
        ephemeral_port_allocation_failed,
        nsime_ip_endpoint_demux:allocate(DemuxState, Callbacks)
    ),
    ?assertEqual(nsime_ip_endpoint_demux:destroy(DemuxState), stopped).

%% Helper Methods %%
parallel_allocate(DemuxState, FirstPort, LastPort, Callbacks) ->
    S = self(),
    TaskID = make_ref(),
    Workers = lists:map(
        fun(_) ->
            spawn(
                fun() ->
                    EndpointState = nsime_ip_endpoint_demux:allocate(DemuxState, Callbacks),
                    ?assert(is_pid(EndpointState)),
                    S ! {self(), TaskID, EndpointState}
                end
            )
        end,
        lists:seq(FirstPort, LastPort)
    ),
    gather(Workers, TaskID).

gather(Workers, TaskID) ->
    case Workers of
        [] ->
            [];
        _ ->
            receive
                {W, TaskID, _Val}->
                NewWorkers = lists:delete(W, Workers),
                gather(NewWorkers, TaskID)
            end
    end.

create_endpoint_callbacks(UdpSocketPid) ->
    {
        {
            nsime_udp_socket,
            forward_up,
            [UdpSocketPid]
        },
        {
            nsime_udp_socket,
            forward_icmp,
            [UdpSocketPid]
        },
        {
            nsime_udp_socket,
            destroy_endpoint,
            [UdpSocketPid]
        }
    }.
