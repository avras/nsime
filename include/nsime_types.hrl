%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : Nsime type definitions
%% Author : Saravanan Vijayakumaran

-type nsime_time_unit() :: sec | milli_sec | micro_sec | nano_sec.

-type nsime_time() :: {number(), nsime_time_unit()}.

-type nsime_data_rate_unit() :: bits_per_sec
                              | bytes_per_sec
                              | kilo_bits_per_sec
                              | kilo_bytes_per_sec
                              | mega_bits_per_sec
                              | mega_bytes_per_sec
                              | giga_bits_per_sec
                              | giga_bytes_per_sec.

-type nsime_data_rate() :: {number(), nsime_data_rate_unit()}.

-type nsime_tx_device_state() :: ready | busy.
