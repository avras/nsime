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

%% Purpose : Test module for nsime_internet_stack_helper
%% Author : Saravanan Vijayakumaran

-module(nsime_internet_stack_helper_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [
            test_install
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_install(_) ->
    nsime_simulator:start(),
    NodePidList = nsime_node:create(2),
    ?assertEqual(nsime_internet_stack_helper:install(NodePidList), ok),
    ?assertError(
        ipv4_already_present,
        nsime_internet_stack_helper:install(NodePidList)
    ),
    nsime_simulator:stop().
