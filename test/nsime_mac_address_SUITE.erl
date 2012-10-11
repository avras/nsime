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

%% Purpose : Test module for nsime_mac_address
%% Author : Saravanan Vijayakumaran

-module(nsime_mac_address_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_mac_address_state.hrl").

all() -> [
            test_start_stop,
            test_allocate,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_start_stop(_) ->
    nsime_mac_address:start(),
    ?assert(lists:member(nsime_mac_address, erlang:registered())),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assertEqual(nsime_mac_address:stop(), stopped),
    ?assertNot(lists:member(nsime_mac_address, erlang:registered())),
    ?assertEqual(nsime_simulator:run(), simulation_complete),
    ?assertEqual(nsime_simulator:stop(), simulation_complete),

    nsime_mac_address:start(),
    ?assert(lists:member(nsime_mac_address, erlang:registered())),
    ?assert(lists:member(nsime_simulator, erlang:registered())),
    ?assertEqual(nsime_simulator:run(), simulation_complete),
    ?assertNot(lists:member(nsime_mac_address, erlang:registered())),
    ?assertEqual(nsime_simulator:stop(), simulation_complete),
    ok.

test_allocate(_) ->
    nsime_mac_address:start(),
    ?assert(lists:member(nsime_mac_address, erlang:registered())),
    ?assertEqual(nsime_mac_address:allocate(), <<1:48>>),
    ?assertEqual(nsime_mac_address:allocate(), <<2:48>>),
    ?assertEqual(nsime_mac_address:allocate(), <<3:48>>),
    ?assertEqual(nsime_mac_address:stop(), stopped),
    ?assertNot(lists:member(nsime_mac_address, erlang:registered())),

    ?assertEqual(nsime_mac_address:allocate(), <<1:48>>),
    ?assertEqual(nsime_mac_address:allocate(), <<2:48>>),
    ?assertEqual(nsime_mac_address:allocate(), <<3:48>>),
    ?assert(lists:member(nsime_mac_address, erlang:registered())),
    ?assertEqual(nsime_mac_address:stop(), stopped),
    ?assertNot(lists:member(nsime_mac_address, erlang:registered())),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).

test_cast_info_codechange(_) ->
    nsime_mac_address:start(),
    Pid = erlang:whereis(nsime_mac_address),
    ?assert(erlang:is_pid(Pid)),
    ?assert(lists:member(nsime_mac_address, erlang:registered())),
    gen_server:cast(nsime_mac_address, junk),
    Pid ! junk,
    nsime_mac_address:code_change(junk, junk, junk),
    ?assertEqual(nsime_mac_address:stop(), stopped),
    ?assertEqual(nsime_simulator:stop(), simulation_complete).
