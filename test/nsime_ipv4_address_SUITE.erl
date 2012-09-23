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

%% Purpose : Test module for nsime_ipv4_address
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_address_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
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
          test_properties,
          test_combine_mask,
          test_subnet_methods,
          test_values,
          test_to_string
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
    ?assertEqual(nsime_ipv4_address:serialize({10, 107, 1, 1}), <<10, 107, 1, 1>>),
    ?assertEqual(nsime_ipv4_address:deserialize(<<10, 107, 1, 1>>), {10, 107, 1, 1}).

test_properties(_) ->
    ?assert(nsime_ipv4_address:is_broadcast({255, 255, 255, 255})),
    ?assertNot(nsime_ipv4_address:is_broadcast({255, 255, 255, 128})),
    ?assert(nsime_ipv4_address:is_multicast({224, 0, 0, 0})),
    ?assert(nsime_ipv4_address:is_multicast({239, 255, 255, 255})),
    ?assertNot(nsime_ipv4_address:is_multicast({255, 255, 255, 128})),
    ?assert(nsime_ipv4_address:is_local_multicast({224, 0, 0, 0})),
    ?assert(nsime_ipv4_address:is_local_multicast({224, 0, 0, 7})),
    ?assert(nsime_ipv4_address:is_local_multicast({224, 0, 0, 255})),
    ?assertNot(nsime_ipv4_address:is_local_multicast({255, 255, 255, 128})).

test_combine_mask(_) ->
    ?assertEqual(nsime_ipv4_address:combine_mask({10, 107, 1, 1}, {255, 255, 255, 255}), {10, 107, 1, 1}),
    ?assertEqual(nsime_ipv4_address:combine_mask({10, 107, 1, 1}, {255, 255, 255, 0}), {10, 107, 1, 0}),
    ?assertEqual(nsime_ipv4_address:combine_mask({10, 107, 1, 1}, {255, 255, 0, 0}), {10, 107, 0, 0}),
    ?assertEqual(nsime_ipv4_address:combine_mask({10, 107, 1, 1}, {255, 0, 0, 0}), {10, 0, 0, 0}).

test_subnet_methods(_) ->
    ?assertError(
        invalid_argument,
        nsime_ipv4_address:get_subnet_directed_broadcast(
            {10, 107, 1, 1},
            {255, 255, 255, 255}
        )
    ),
    ?assertEqual(
        nsime_ipv4_address:get_subnet_directed_broadcast(
            {10, 107, 1, 1},
            {255, 255, 255, 0}
        ),
        {10, 107, 1, 255}
    ),
    ?assertError(
        invalid_argument,
        nsime_ipv4_address:is_subnet_directed_broadcast(
            {10, 107, 1, 1},
            {255, 255, 255, 255}
        )
    ),
    ?assert(
        nsime_ipv4_address:is_subnet_directed_broadcast(
            {10, 107, 1, 255},
            {255, 255, 255, 0}
        )
    ).

test_values(_) ->
    ?assertEqual(nsime_ipv4_address:get_zero(), {0, 0, 0, 0}),
    ?assertEqual(nsime_ipv4_address:get_any(), {0, 0, 0, 0}),
    ?assertEqual(nsime_ipv4_address:get_broadcast(), {255, 255, 255, 255}),
    ?assertEqual(nsime_ipv4_address:get_loopback(), {127, 0, 0, 1}).

test_to_string(_) ->
    ?assertEqual(nsime_ipv4_address:to_string({10, 107, 1, 1}), "10.107.1.1").
