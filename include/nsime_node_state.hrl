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

%% Purpose : Node state record
%% Author : Saravanan Vijayakumaran

-record(nsime_protocol_handler_record,
        {
          handler                 :: nsime_callback(),
          device                  :: pid(),
          protocol                :: 0..65535,
          promiscuous             :: boolean()
        }).

-record(nsime_node_state,
        {
          applications = []       :: [pid()],
          netdevices = []         :: [pid()],
          protocol_handlers = []  :: [#nsime_protocol_handler_record{}],
          objects = []            :: list()
        }).
