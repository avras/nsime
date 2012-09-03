%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : UDP echo server state record
%% Author : Saravanan Vijayakumaran

-record(nsime_udp_echo_server_state,
        {
          node                        :: pid(),
          socket                      :: pid(),
          listen_port                 :: inet:port_number(),
          transmit_trace_callback     :: fun(),
          receive_trace_callback      :: fun()
        }).
