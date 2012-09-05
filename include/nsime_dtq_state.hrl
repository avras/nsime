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

%% Purpose : Node drop tail queue state record
%% Author : Saravanan Vijayakumaran

-record(nsime_dtq_state,
        {
          current_packet_count = 0      :: integer(),
          current_byte_count = 0        :: integer(),
          received_packet_count = 0     :: integer(),
          received_byte_count = 0       :: integer(),
          dropped_packet_count = 0      :: integer(),
          dropped_byte_count = 0        :: integer(),
          max_byte_count = 0            :: integer(),
          max_packet_count = 0          :: integer(),
          device_id = undefined         :: pid(),
          packets = queue:new()         :: queue()
        }).
