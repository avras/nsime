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

%% Purpose : Test module for nsime_config
%% Author : Saravanan Vijayakumaran

-module(nsime_config_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("nsime_config_state.hrl").

all() -> [
            test_start_stop,
            test_checksum_methods,
            test_cast_info_codechange
         ].


init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_start_stop(_) ->
    nsime_config:start(),
    ?assert(lists:member(nsime_config, erlang:registered())),
    ?assertEqual(nsime_config:stop(), stopped),
    ?assertNot(lists:member(nsime_config, erlang:registered())).

test_checksum_methods(_) ->
    nsime_config:start(),
    ?assert(lists:member(nsime_config, erlang:registered())),
    ?assertEqual(nsime_config:enable_checksum(), ok),
    ?assert(nsime_config:checksum_enabled()),
    ?assertEqual(nsime_config:disable_checksum(), ok),
    ?assertNot(nsime_config:checksum_enabled()),
    ?assertEqual(nsime_config:stop(), stopped).

test_cast_info_codechange(_) ->
    nsime_config:start(),
    Pid = erlang:whereis(nsime_config),
    ?assert(erlang:is_pid(Pid)),
    ?assert(lists:member(nsime_config, erlang:registered())),
    gen_server:cast(nsime_config, junk),
    Pid ! junk,
    nsime_config:code_change(junk, junk, junk),
    ?assertEqual(nsime_config:stop(), stopped).
