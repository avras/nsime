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

%% Purpose :IPv4 header state record
%% Author : Saravanan Vijayakumaran

-record(nsime_ipv4_header_state,
        {
          calculate_checksum = false      :: boolean(),
          payload_size = 0                :: non_neg_integer(),
          identification = 0              :: non_neg_integer(),
          tos = 0                         :: non_neg_integer(),
          ttl = 0                         :: non_neg_integer(),
          protocol = 0                    :: non_neg_integer(),
          flags = 0                       :: non_neg_integer(),
          fragment_offset = 0             :: non_neg_integer(),
          source_address                  :: inet:ip4_address(),
          destination_address             :: inet:ip4_address(),
          checksum = 0                    :: non_neg_integer(),
          checksum_correct = false        :: boolean(),
          header_size = 20                :: non_neg_integer()
        }).
