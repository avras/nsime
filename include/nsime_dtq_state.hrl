%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Node drop tail queue state record
%% Author : Saravanan Vijayakumaran

-record(nsime_dtq_state,
        {
          current_packet_count = 0      :: integer(),
          current_byte_count = 0        :: integer(),
          received_packet_count = 0     :: integer(),
          received_byte_count = 0       :: integer(),
          dropped_packet_count = 0      :: integer(),
          dropped_byte_count = 0        :: integer(),
          max_byte_count = 0                 :: integer(),
          max_packet_count = 0               :: integer(),
          device_id = undefined         :: pid(),
          packets                       :: queue()
        }).
