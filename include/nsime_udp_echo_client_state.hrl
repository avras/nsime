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
          socket                      :: pid(),
          peer_address                :: inet:ip_address(),
          peer_port                   :: inet:port_number(),
          data_size                   :: integer(),
          data                        :: binary(),
          num_sent_packets = 0        :: integer(),
          max_packets = infinity      :: integer(),
          send_event                  :: nsime_event(),
          transmit_callback           :: fun(),
          receive_callback            :: fun()
        }).
