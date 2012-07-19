%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Simulator module
%% Author : Saravanan Vijayakumaran

-module(nsime_simulator).
-author("Saravanan Vijayakumaran").

-export([init/0, init/1, run/0, stop/0]).
-export([schedule/1, cancel/0]).
-export([current_time/0]).

init() ->
  ok.

%% Argument will be the type of scheduler(map, list, heap)
init(_) ->
  ok.

run() ->
  ok.

stop() ->
  ok.

schedule(_) ->
  ok.

cancel() ->
  ok.

current_time() ->
  ok.
