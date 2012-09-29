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

%% Purpose : IPv4 protocol state record
%% Author : Saravanan Vijayakumaran

-define(IPv4_PROTOCOL_NUMBER, 16#0800).

-record(nsime_ipv4_protocol_state,
        {
          node                                      :: pid(),
          layer4_protocols = []                     :: [pid()],
          interfaces = []                           :: [pid()],
          routing_protocol                          :: pid(),
          raw_sockets = []                          :: [pid()],
          ip_forward = false                        :: boolean(),
          weak_es_model = false                     :: boolean(),
          default_ttl = 64                          :: 0..255,
          identification = 0                        :: 0..65535,
          send_outgoing_trace                       :: nsime_callback(),
          unicast_forward_trace                     :: nsime_callback(),
          local_deliver_trace                       :: nsime_callback(),
          transmit_trace                            :: nsime_callback(),
          receive_trace                             :: nsime_callback(),
          drop_trace                                :: nsime_callback(),
          fragments = []                            :: list(),
          fragment_expiration_timeout = {30, sec}   :: nsime_time(),
          fragment_timers = []                      :: [#nsime_event{}]
        }).
