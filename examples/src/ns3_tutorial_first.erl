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

%% Purpose : nsime port of ns3/examples/tutorial/first.cc
%% Author : Saravanan Vijayakumaran

-module(ns3_tutorial_first).
-author("Saravanan Vijayakumaran").

-export([run/0]).

run() ->
    nsime_simulator:start(),
    NodePidList = nsime_node:create(2),

    PtpHelperPid = nsime_ptp_helper:create(),
    nsime_ptp_helper:call_on_device(PtpHelperPid, set_data_rate, {5, mega_bits_per_sec}),
    nsime_ptp_helper:call_on_channel(PtpHelperPid, set_channel_delay, {2, milli_sec}),

    nsime_ptp_helper:install(PtpHelperPid, NodePidList),
    nsime_internet_stack_helper:install(NodePidList),

    nsime_simulator:run(),

    nsime_ptp_helper:destroy(PtpHelperPid),
    nsime_simulator:stop().
