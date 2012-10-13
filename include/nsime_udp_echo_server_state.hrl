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

%% Purpose : UDP echo server state record
%% Author : Saravanan Vijayakumaran

-record(nsime_udp_echo_server_state,
        {
          node                                            :: pid(),
          socket                                          :: pid(),
          listen_port                                     :: inet:port_number(),
          transmit_trace_callback = {none, none, none}    :: nsime_callback(),
          receive_trace_callback = {none, none, none}     :: nsime_callback()
        }).
