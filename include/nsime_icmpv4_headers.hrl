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

%% Purpose : ICMPv4 header records
%% Author : Saravanan Vijayakumaran

-define(ICMPv4_ECHO_REPLY, 0).
-define(ICMPv4_DEST_UNREACH, 3).
-define(ICMPv4_ECHO, 8).
-define(ICMPv4_TIME_EXCEEDED, 11).

-record(nsime_icmpv4_header,
        {
          type = 0                        :: 0..255,
          code = 0                        :: 0..255,
          calculate_checksum = false      :: boolean()
        }).

-record(nsime_icmpv4_echo_header,
        {
          identifier = 0                  :: 0..65535,
          sequence = 0                    :: 0..65535,
          data                            :: binary(),
          data_size = 0                   :: non_neg_integer()
        }).


-define(ICMPv4_NET_UNREACHABLE, 0).
-define(ICMPv4_HOST_UNREACHABLE, 1).
-define(ICMPv4_PROTOCOL_UNREACHABLE, 2).
-define(ICMPv4_PORT_UNREACHABLE, 3).
-define(ICMPv4_FRAG_NEEDED, 4).
-define(ICMPv4_SOURCE_ROUTE_FAILED, 5).

-record(nsime_icmpv4_destination_unreachable_header,
        {
          next_hop_mtu = 0                :: 0..65535,
          header                          :: #nsime_ipv4_header{},
          data                            :: binary()
        }).


-define(ICMPv4_TIME_TO_LIVE, 0).
-define(ICMPv4_FRAGMENT_REASSEMBLY, 1).

-record(nsime_icmpv4_time_exceeded_header,
        {
          next_hop_mtu = 0                :: 0..65535,
          header                          :: #nsime_ipv4_header{},
          data                            :: binary()
        }).
