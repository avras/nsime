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

%% Purpose : Layer 4 protocol module
%% Author : Saravanan Vijayakumaran

-module(nsime_layer4_protocol).
-author("Saravanan Vijayakumaran").

-export([protocol_number/1, recv/4, recv_icmp/9]).

protocol_number(ProtocolPid) ->
    gen_server:call(ProtocolPid, protocol_number).

recv(ProtocolPid, Packet, Header, Interface) ->
    gen_server:call(ProtocolPid, {recv, Packet, Header, Interface}).

recv_icmp(
    ProtocolPid,
    IcmpSource,
    IcmpTTL,
    IcmpType,
    IcmpCode,
    IcmpInfo,
    PayloadSource,
    PayloadDestination,
    Payload
) ->
    gen_server:call(ProtocolPid, {recv_icmp,
                                  IcmpSource,
                                  IcmpTTL,
                                  IcmpType,
                                  IcmpCode,
                                  IcmpInfo,
                                  PayloadSource,
                                  PayloadDestination,
                                  Payload
                                 }).
