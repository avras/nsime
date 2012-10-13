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

%% Purpose : UDP socket state record
%% Author : Saravanan Vijayakumaran

-define(MAX_IPv4_UDP_DATAGRAM_SIZE, 65507).

-record(nsime_udp_socket_state,
        {
          bound_netdevice                             :: pid(),
          receive_packet_info = false                 :: boolean(),
          ip_endpoint                                 :: pid(),
          node                                        :: pid(),
          udp_protocol                                :: pid(),
          default_address                             :: inet:ip_address(),
          default_port                                :: inet:port_number(),
          drop_trace_callback = {none, none, none}    :: nsime_callback(),
          socket_error = error_noterror               :: nsime_socket_error(),
          shutdown_send = false                       :: boolean(),
          shutdown_receive = false                    :: boolean(),
          connected = false                           :: boolean(),
          allow_broadcast = false                     :: boolean(),
          delivery_queue = queue:new()                :: queue(),
          received_available = 0                      :: non_neg_integer(),
          receive_buffer_size = infinity              :: non_neg_integer(),
          ttl = 0                                     :: non_neg_integer(),
          multicast_ttl = 0                           :: non_neg_integer(),
          multicast_interface                         :: integer(),
          multicast_loop = false                      :: boolean(),
          mtu_discover = false                        :: boolean(),
          icmp_callback                               :: nsime_callback(),
          receive_callback                            :: nsime_callback()
        }).
