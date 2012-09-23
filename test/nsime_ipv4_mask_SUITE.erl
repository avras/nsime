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

%% Purpose : Test module for nsime_ipv4_mask
%% Author : Saravanan Vijayakumaran

-module(nsime_ipv4_mask_SUITE).
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
          test_creation,
          test_prefix_length,
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

test_creation(_) ->
    ?assertEqual(nsime_ipv4_mask:create("255.0.0.0"), {255, 0, 0, 0}),
    ?assertEqual(nsime_ipv4_mask:create("255.255.0.0"), {255, 255, 0, 0}),
    ?assertEqual(nsime_ipv4_mask:create("255.255.255.0"), {255, 255, 255, 0}),
    ?assertEqual(nsime_ipv4_mask:create("255.255.255.255"), {255, 255, 255, 255}),
    ?assertError(invalid_argument, nsime_ipv4_mask:create("junk")).

test_prefix_length(_) ->
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({128, 0, 0, 0}), 1),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({192, 0, 0, 0}), 2),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({224, 0, 0, 0}), 3),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({240, 0, 0, 0}), 4),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({248, 0, 0, 0}), 5),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({252, 0, 0, 0}), 6),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({254, 0, 0, 0}), 7),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({255, 0, 0, 0}), 8),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({255, 128, 0, 0}), 9),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({255, 192, 0, 0}), 10),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({255, 224, 0, 0}), 11),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({255, 240, 0, 0}), 12),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({255, 248, 0, 0}), 13),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({255, 255, 0, 0}), 16),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({255, 255, 255, 0}), 24),
    ?assertEqual(nsime_ipv4_mask:get_prefix_length({255, 255, 255, 255}), 32).

test_values(_) ->
    ?assertEqual(nsime_ipv4_mask:get_loopback(), {255, 0, 0, 0}),
    ?assertEqual(nsime_ipv4_mask:get_zero(), {0, 0, 0, 0}),
    ?assertEqual(nsime_ipv4_mask:get_ones(), {255, 255, 255, 255}).

test_to_string(_) ->
    ?assertEqual(nsime_ipv4_mask:to_string({255, 255, 255, 255}), "255.255.255.255").
