%%
%%  Copyright (C) 2012 Saravanan Vijayakumaran <sarva.v@gmail.com>
%%
%%  This file is part of nsime.
%%
%%  nsime is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  nsime is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with nsime.  If not, see <http://www.gnu.org/licenses/>.
%%

%% Purpose : Simulation time module
%% Author : Saravanan Vijayakumaran

-module(nsime_time).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").

-export([add/2, value/1]).
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
    is_number(Value),
    Value >= 0
        -> is_nsime_time_unit(Unit);
is_nsime_time({infinity, Unit}) ->
    is_nsime_time_unit(Unit);
is_nsime_time(_) ->
    false.

add(A, B) ->
    case is_nsime_time(A) andalso is_nsime_time(B) of
        true ->
            case {A,B} of
                {{infinity, _Unit1}, {_ValB, _Unit2}} ->
                    {infinity, sec};
                {{_ValA, _Unit1}, {infinity, _Unit2}} ->
                    {infinity, sec};
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

value(Time) ->
    case {is_nsime_time(Time), Time} of
        {true, {infinity, _Unit}} ->
            infinity;
        {true, {Value, sec}} ->
            Value*1000000000;
        {true, {Value, milli_sec}} ->
            Value*1000000;
        {true, {Value, micro_sec}} ->
            Value*1000;
        {true, {Value, nano_sec}} ->
            Value;
        {false, _} ->
            erlang:error(invalid_argument)
    end.
