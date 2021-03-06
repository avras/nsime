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

%% Purpose : Point-to-point netdevice state record
%% Author : Saravanan Vijayakumaran

-record(nsime_ptp_netdevice_state,
        {
            device_type = nsime_ptp_netdevice             :: atom(),
            node                                          :: pid(),
            address                                       :: binary(),
            receive_callback                              :: nsime_callback(),
            promisc_receive_callback = {none, none, none} :: nsime_callback(),
            tx_state = ready                              :: nsime_tx_device_state(),
            data_rate                                     :: nsime_data_rate(),
            interframe_gap = {0, sec}                     :: nsime_time(),
            channel                                       :: pid(),
            queue_module = nsime_drop_tail_queue          :: module(),
            queue_state                                   :: undefined | #nsime_droptail_queue_state{},
            receive_error_model                           :: pid(),
            mac_tx_trace = {none, none, none}             :: nsime_callback(),
            mac_tx_drop_trace = {none, none, none}        :: nsime_callback(),
            mac_promisc_rx_trace = {none, none, none}     :: nsime_callback(),
            mac_rx_trace = {none, none, none}             :: nsime_callback(),
            mac_rx_drop_trace = {none, none, none}        :: nsime_callback(),
            phy_tx_begin_trace = {none, none, none}       :: nsime_callback(),
            phy_tx_end_trace = {none, none, none}         :: nsime_callback(),
            phy_tx_drop_trace = {none, none, none}        :: nsime_callback(),
            phy_rx_begin_trace = {none, none, none}       :: nsime_callback(),
            phy_rx_end_trace = {none, none, none}         :: nsime_callback(),
            phy_rx_drop_trace = {none, none, none}        :: nsime_callback(),
            sniffer_trace = {none, none, none}            :: nsime_callback(),
            promisc_sniffer_trace = {none, none, none}    :: nsime_callback(),
            link_up = false                               :: boolean(),
            mtu = 1500                                    :: pos_integer(),
            current_packet = none                         :: none | #nsime_packet{},
            interface                                     :: reference()
        }).
