%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Queue module
%% Author : Saravanan Vijayakumaran

-module(nsime_queue).
-author("Saravanan Vijayakumaran").

-export([create/0, destroy/1, isempty/1]).
-export([enqueue_packet/2, dequeue_packet/1, drop_packet/2, dequeue_all_packets/1]).
-export([get_packet_count/1, get_byte_count/1]).
-export([get_total_received_packets/1, get_total_received_bytes/1]).
-export([get_total_dropped_packets/1, get_total_dropped_bytes/1]).
-export([reset_statistics/1]).

create() ->
  loop([]),
  ok.

%% Argument will be the pid of queue to destroy
destroy(_) ->
  ok.

%% Argument will be the pid of queue
isempty(_) ->
  ok.

%% Argument 1 will be the pid of queue
%% Argument 2 will be the packet to enqueue
enqueue_packet(_,_) ->
  ok.

%% Argument will be the pid of queue
dequeue_packet(_) ->
  ok.

%% Argument 1 will be the pid of queue
%% Argument 2 will be the packet to drop
drop_packet(_,_) ->
  ok.

%% Argument will be the pid of queue
dequeue_all_packets(_) ->
  ok.

%% Argument will be the pid of queue
get_packet_count(_) ->
  ok.

%% Argument will be the pid of queue
get_byte_count(_) ->
  ok.

%% Argument will be the pid of queue
get_total_received_packets(_) ->
  ok.

%% Argument will be the pid of queue
get_total_received_bytes(_) ->
  ok.

%% Argument will be the pid of queue
get_total_dropped_packets(_) ->
  ok.

%% Argument will be the pid of queue
get_total_dropped_bytes(_) ->
  ok.

%% Argument will be the pid of queue
reset_statistics(_) ->
  ok.

loop(State) ->
  loop(State).
