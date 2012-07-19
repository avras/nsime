%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Application module
%% Author : Saravanan Vijayakumaran

-module(nsime_application).
-author("Saravanan Vijayakumaran").

-export([create/0, destroy/1]).
-export([set_start_time/1, set_stop_time/1]).
-export([get_node/1, set_node/1]).

create() ->
  loop([]),
  ok.

%% Argument pid of application to destroy
destroy(_) ->
  ok.

%% Argument will be a time value
set_start_time(_) ->
  ok.

%% Argument will be a time value
set_stop_time(_) ->
  ok.

%% Argument will be pid of a node
get_node(_) ->
  ok.

%% Argument will be pid of a node
set_node(_) ->
  ok.

loop(State) ->
  loop(State).
