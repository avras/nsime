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

%% Purpose : IPv4 routing table entry record
%% Author : Saravanan Vijayakumaran

-record(nsime_ipv4_routing_table_entry,
        {
          destination                             :: inet:ip4_address(),
          network_mask = {255, 255, 255, 255}     :: nsime_ipv4_mask(),
          gateway = {0, 0, 0, 0}                  :: inet:ip4_address(),
          interface                               :: reference(),
          metric = 0                              :: non_neg_integer()
        }).
