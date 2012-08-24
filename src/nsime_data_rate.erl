%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Data rate module
%% Author : Saravanan Vijayakumaran

-module(nsime_data_rate).
-author("Saravanan Vijayakumaran").

-include("nsime_types.hrl").

-export([is_nsime_data_rate_unit/1, is_nsime_data_rate/1]).
-export([calc_tx_time/2]).

is_nsime_data_rate_unit(X) ->
    case X of
        bits_per_sec -> true;
        bytes_per_sec -> true;
        kilo_bits_per_sec -> true;
        kilo_bytes_per_sec -> true;
        mega_bits_per_sec -> true;
        mega_bytes_per_sec -> true;
        giga_bits_per_sec -> true;
        giga_bytes_per_sec -> true;
        _ -> false
    end.

is_nsime_data_rate({Value, Unit}) when
    is_number(Value),
    Value >= 0
        -> is_nsime_data_rate_unit(Unit);
is_nsime_data_rate(_) ->
    false.

calc_tx_time(DataRate, NumBytes) ->
    case is_nsime_data_rate(DataRate) andalso 
         is_number(NumBytes) andalso
         NumBytes >= 0 of
        true ->
            {Value, Unit} = DataRate,
            case Unit of
                bits_per_sec -> {NumBytes*8/Value, sec};
                bytes_per_sec -> {NumBytes/Value, sec};
                kilo_bits_per_sec -> {NumBytes*8/(Value*1000), sec};
                kilo_bytes_per_sec -> {NumBytes/(Value*1000), sec};
                mega_bits_per_sec -> {NumBytes*8/(Value*1000000), sec};
                mega_bytes_per_sec -> {NumBytes/(Value*1000000), sec};
                giga_bits_per_sec -> {NumBytes*8/(Value*1000000000), sec};
                giga_bytes_per_sec -> {NumBytes/(Value*1000000000), sec}
            end;
        false ->
            erlang:error(invalid_argument)
    end.
