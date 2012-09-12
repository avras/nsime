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

%% Purpose :IPv4 header record
%% Author : Saravanan Vijayakumaran

-record(nsime_ipv4_header,
        {
          header_length = 5               :: 5..15,
          tos = 0                         :: 0..255,
          total_length = 0                :: 0..65535,
          identification = 0              :: 0..65535,
          flags = 0                       :: 0..7,
          fragment_offset = 0             :: 0..8191,
          ttl = 0                         :: 0..255,
          protocol = 0                    :: 0..255,
          checksum = 0                    :: non_neg_integer(),
          source_address                  :: inet:ip4_address(),
          destination_address             :: inet:ip4_address(),
          calculate_checksum = false      :: boolean(),
          checksum_correct = false        :: boolean()
        }).
