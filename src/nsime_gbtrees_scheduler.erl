%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Scheduler module based on gb_trees
%% Author : Saravanan Vijayakumaran

-module(nsime_gbtrees_scheduler).
-author("Saravanan Vijayakumaran").
-export([create/0, insert/1, is_empty/0, remove/1, remove_next/0]).

-behaviour(nsime_scheduler).

create() ->
    ok.

insert(_) ->
    ok.

is_empty() ->
    ok.

remove(_) ->
    ok.

remove_next() ->
    ok.
