%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : UDP echo client state record
%% Author : Saravanan Vijayakumaran

-record(nsime_udp_echo_client_state,
        {
          packet_size                 :: integer(),
          packet_data                 :: binary(),
          num_sent_packets            :: integer(),
          max_packets                 :: integer(),
          socket                      :: module(),
          peer_address                :: inet:ip_address(),
          peer_port                   :: inet:port_number(),
          send_event                  :: nsime_event(),
          transmit_callback           :: fun(),
          receive_callback            :: fun()
        }).
