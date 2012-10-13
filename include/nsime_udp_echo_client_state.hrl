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

%% Purpose : UDP echo client state record
%% Author : Saravanan Vijayakumaran

-record(nsime_udp_echo_client_state,
        {
          node                                            :: pid(),
          socket                                          :: pid(),
          peer_address                                    :: inet:ip_address(),
          peer_port                                       :: inet:port_number(),
          data_size = 0                                   :: integer(),
          data = undefined                                :: binary(),
          num_sent_packets = 0                            :: integer(),
          max_packets = infinity                          :: integer(),
          inter_packet_gap = {0, sec}                     :: nsime_time(),
          send_event                                      :: #nsime_event{},
          transmit_trace_callback = {none, none, none}    :: nsime_callback(),
          receive_trace_callback = {none, none, none}     :: nsime_callback()
        }).
