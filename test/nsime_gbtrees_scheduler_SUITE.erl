%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Test module for nsime_gbtrees_scheduler
%% Author : Saravanan Vijayakumaran

-module(nsime_gbtrees_scheduler_SUITE).
-author("Saravanan Vijayakumaran").

-compile(export_all).

-include("ct.hrl").

all() -> [test_scheduler_creation].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

test_scheduler_creation(Config) ->
    ok.
    
