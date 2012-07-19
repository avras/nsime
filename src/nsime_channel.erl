%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Channel module
%% Author : Saravanan Vijayakumaran

-module(nsime_channel).
-author("Saravanan Vijayakumaran").

-export([create/0, destroy/1]).
-export([get_netdevice/1, get_netdevice_count/1]).
-export([send/0]).


create() ->
  loop([]),
  ok.

%% Argument pid of channel to destroy
destroy(_) ->
  ok.

%% Argument will be index of netdevice
get_netdevice(_) ->
  ok.

%% Argument will be pid of a channel
get_netdevice_count(_) ->
  ok.

send() ->
  ok.

loop(State) ->
  loop(State).
