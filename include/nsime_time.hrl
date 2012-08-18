%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Time type definition
%% Author : Saravanan Vijayakumaran

-type nsime_time_unit() :: sec | milli_sec | micro_sec | nano_sec.

-type nsime_time() :: {non_neg_integer(), nsime_time_unit()}. 
