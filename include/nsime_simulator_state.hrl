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

%% Purpose : Simulator state record
%% Author : Saravanan Vijayakumaran

-record(nsime_simulator_state,
        {
          current_time = {0, sec}     :: nsime_time(),
          scheduler                   :: module(),
          scheduler_state             :: gb_tree() | list(),
          num_remaining_events = 0    :: integer(),
          num_executed_events = 0     :: integer(),
          stopped = false             :: boolean()
        }).
