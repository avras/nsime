%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Simulation event record
%% Author : Saravanan Vijayakumaran

-record(nsime_event,
        {
          time            :: integer(),
          pid             :: pid(),
          module          :: module(),
          function        :: fun(),
          arguments=[]    :: list(),
          eventid         :: reference()
        }).
