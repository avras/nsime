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

%% Purpose : Application module
%% Author : Saravanan Vijayakumaran

-module(nsime_application).
-author("Saravanan Vijayakumaran").

-export([destroy/1, set_node/2, get_node/1, schedule_start/2,
         start/1, stop/1]).

destroy(ApplicationPid) ->
    gen_server:call(ApplicationPid, terminate).

set_node(ApplicationPid, NodePid) ->
    gen_server:call(ApplicationPid, {set_node, NodePid}).

get_node(ApplicationPid) ->
    gen_server:call(ApplicationPid, get_node).

schedule_start(ApplicationPid, Time) ->
    gen_server:call(ApplicationPid, {schedule_start, Time}).

start(ApplicationPid) ->
    gen_server:call(ApplicationPid, start).

stop(ApplicationPid) ->
    gen_server:call(ApplicationPid, stop).


