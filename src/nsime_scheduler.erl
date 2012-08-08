%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Scheduler Behaviour
%% Author : Saravanan Vijayakumaran

-module(nsime_scheduler).
-author("Saravanan Vijayakumaran").

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
      {create, 0},
      {insert, 1},
      {is_empty, 0},
      {remove, 1},
      {remove_next, 0}
    ];
behaviour_info(_Other) ->
    undefined.
