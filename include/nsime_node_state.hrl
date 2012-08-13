%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Node state record
%% Author : Saravanan Vijayakumaran

-record(nsime_node_state,
        {
          applications = [],
          netdevices = []
        }).
