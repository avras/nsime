%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Point-to-point channel state record
%% Author : Saravanan Vijayakumaran

-record(nsime_ptp_channel_state,
        {
          delay = {0, sec}         :: nsime_time(),
          devices = {none, none}   :: {none, none} | {pid(), none} | {pid(), pid()}
        }).
