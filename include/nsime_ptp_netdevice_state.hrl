%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Point-to-point netdevice state record
%% Author : Saravanan Vijayakumaran

-record(nsime_ptp_netdevice_state,
        {
            node                                      :: pid(),
            address                                   :: binary(),
            tx_state = ready                          :: nsime_tx_device_state(),
            data_rate                                 :: nsime_data_rate(),
            interframe_gap                            :: nsime_time(),
            channel                                   :: pid(),
            queue_module = nsime_drop_tail_queue      :: module(),
            queue                                     :: pid(),
            rx_error_model                            :: module(),
            link_up = false                           :: boolean(),
            mtu = 1500                                :: pos_integer(),
            current_packet = none                     :: none | #nsime_packet{},
            device_index                              :: non_neg_integer()
        }).
