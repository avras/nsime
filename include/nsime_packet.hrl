%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Packet record
%% Author : Saravanan Vijayakumaran

-record(nsime_packet,
        {
            id          :: reference(),
            size        :: integer(),
            data        :: binary()
        }).
