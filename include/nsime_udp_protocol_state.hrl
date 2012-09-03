%%
%% %CopyrightBegin%
%%
%% Copyright Saravanan Vijayakumaran 2012. All Rights Reserved.
%%
%% %CopyrightEnd%
%%

%% Purpose : UDP protocol state record
%% Author : Saravanan Vijayakumaran

-record(nsime_udp_protocol_state,
        {
          node                      :: pid(),
          ipv4_endpoints = []       :: list(),
          ipv6_endpoints = []       :: list(),
          sockets = []              :: list(),
          layer3_send_ipv4          :: {module(), fun()},
          layer3_send_ipv6          :: {module(), fun()}
        }).
