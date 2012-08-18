%%
%% %CopyrightBegin%
%% 
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%% 
%% %CopyrightEnd%
%%

%% Purpose : Simulation time module
%% Author : Saravanan Vijayakumaran

-module(nsime_time).
-author("Saravanan Vijayakumaran").

-include("nsime_time.hrl").

-export([add/2]).
-export([is_nsime_time_unit/1, is_nsime_time/1]).

is_nsime_time_unit(X) -> 
    case X of
        sec -> true;
        milli_sec -> true;
        micro_sec -> true;
        nano_sec -> true;
        _ -> false
    end.

is_nsime_time({Value, Unit}) when 
    is_integer(Value),
    Value >= 0
        -> is_nsime_time_unit(Unit);
is_nsime_time(_) ->
    false.

add(A, B) ->
    case is_nsime_time(A) andalso is_nsime_time(B) of
        true ->
            case {A,B} of
                {{ValA, Unit}, {ValB, Unit}} -> 
                    {ValA + ValB, Unit};
                {{ValA, sec}, {ValB, UnitB}} ->
                    case UnitB of
                        milli_sec ->
                            {ValA*1000+ValB, milli_sec};
                        micro_sec ->
                            {ValA*1000000+ValB, micro_sec};
                        nano_sec ->
                            {ValA*1000000000+ValB, nano_sec}
                    end;
                {{ValA, milli_sec}, {ValB, UnitB}} ->
                    case UnitB of
                        sec ->
                            {ValA+ValB*1000, milli_sec};
                        micro_sec ->
                            {ValA*1000+ValB, micro_sec};
                        nano_sec ->
                            {ValA*1000000+ValB, nano_sec}
                    end;
                {{ValA, micro_sec}, {ValB, UnitB}} ->
                    case UnitB of
                        sec ->
                            {ValA+ValB*1000000, micro_sec};
                        milli_sec ->
                            {ValA+ValB*1000, micro_sec};
                        nano_sec ->
                            {ValA*1000+ValB, nano_sec}
                    end;
                {{ValA, nano_sec}, {ValB, UnitB}} ->
                    case UnitB of
                        sec ->
                            {ValA+ValB*1000000000, nano_sec};
                        milli_sec ->
                            {ValA+ValB*1000000, nano_sec};
                        micro_sec ->
                            {ValA+ValB*1000, nano_sec}
                    end
            end;
        false ->
            erlang:error(invalid_argument)
    end.
