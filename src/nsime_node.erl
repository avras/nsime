%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Node module
%% Author : Saravanan Vijayakumaran

-module(nsime_node).
-author("Saravanan Vijayakumaran").

-export([create/0, create/1, destroy/1]).
-export([add_netdevice/1, get_netdevice/1, get_netdevice_count/1]).
-export([add_application/1, get_application/1, get_application_count/1]).

create() ->
  loop([]),
  ok.

%% Argument will be number of nodes
create(_) ->
  ok.

%% Argument pid of node to destroy
%% Will be removed from global node list
%% and the corresponding process killed
destroy(_) ->
  ok.

%% Argument will be netdevice type (parametrized module?)
add_netdevice(_) ->
  ok.

%% Argument will be an index
get_netdevice(_) ->
  ok.

%% Argument will be pid of a node
get_netdevice_count(_) ->
  ok.

%% Argument will be application type (parametrized module?)
add_application(_) ->
  ok.

%% Argument will be an index
get_application(_) ->
  ok.

%% Argument will be pid of a node
get_application_count(_) ->
  ok.

loop(State) ->
  ok.
