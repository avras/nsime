%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Simulator state record
%% Author : Saravanan Vijayakumaran

-record(nsime_simulator_state,
        {
          current_time = 0            :: integer(),
          scheduler                   :: module(),
          num_remaining_events = 0    :: integer(),
          num_executed_events = 0     :: integer(),
          stopped = false             :: boolean()
        }).
