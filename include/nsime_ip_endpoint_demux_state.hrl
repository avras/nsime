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

%% Purpose :IP endpoint demultiplexer state record
%% Author : Saravanan Vijayakumaran

-record(nsime_ip_endpoint_demux_state,
        {
          demux_id                    :: reference(),
          ephemeral_port = 49152      :: inet:port_number(),
          first_port = 49152          :: inet:port_number(),
          last_port = 65535           :: inet:port_number(),
          endpoints = []              :: [#nsime_ip_endpoint_state{}]
        }).
