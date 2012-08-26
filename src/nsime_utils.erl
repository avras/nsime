%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Nsime utilities
%% Author : Saravanan Vijayakumaran

-module(nsime_utils).
-author("Saravanan Vijayakumaran").

-export([wait_for_registration/1]).

wait_for_registration(ProcessList) ->
    case lists:foldl(
        fun(ProcessName, State) -> 
            lists:member(ProcessName, registered()) and State 
        end, 
        true, 
        ProcessList
    ) of
        true -> 
            ok;
        false ->
            wait_for_registration(ProcessList)
    end.

