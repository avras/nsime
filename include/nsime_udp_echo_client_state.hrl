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
          node                        :: pid(),
          socket                      :: pid(),
          peer_address                :: inet:ip_address(),
          peer_port                   :: inet:port_number(),
          data_size = 0               :: integer(),
          data = undefined            :: binary(),
          num_sent_packets = 0        :: integer(),
          max_packets = infinity      :: integer(),
          inter_packet_gap = {0, sec} :: nsime_time(),
          send_event                  :: #nsime_event{},
          transmit_trace_callback     :: fun(),
          receive_trace_callback      :: fun()
        }).
