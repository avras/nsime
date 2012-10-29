%% Purpose : Utility functions for parallel list processing
%% Author : Saravanan Vijayakumaran
%% Source: http://montsamu.blogspot.in/2007/02/erlang-parallel-map-and-parallel.html

-module(plist).

-export([parmap/2]).
-export([pmap/2,pforeach/2,npforeach/2]).
-export([pforeach_orderless/2]).

parmap(Function, List) ->
    Parent = self(),
    [receive {Pid, Result} -> Result end
    || Pid <- [spawn(fun() -> Parent ! {self(), Function(X)} end) || X <- List]].


pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
    pmap_gather(Pids).

pmap_gather([H|T]) ->
    receive
        {H, Ret} -> [Ret|pmap_gather(T)]
    end;
pmap_gather([]) ->
    [].

pmap_f(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.

pforeach(F, L) ->
    S = self(),
    Pids = pmap(fun(I) -> spawn(fun() -> pforeach_f(S,F,I) end) end, L),
    pforeach_wait(Pids).

pforeach_wait([H|T]) ->
    receive
      H -> pforeach_wait(T)
    end;
pforeach_wait([]) -> ok.

pforeach_f(Parent, F, I) ->
    _ = (catch F(I)),
    Parent ! self().

npforeach(F, L) ->
    S = self(),
    Pid = spawn(fun() -> npforeach_0(S,F,L) end),
    receive Pid -> ok end.

npforeach_0(Parent,F,L) ->
    S = self(),
    Pids = pmap(fun(I) -> spawn(fun() -> npforeach_f(S,F,I) end) end, L),
    npforeach_wait(S,length(Pids)),
    Parent ! S.

npforeach_wait(_S,0) -> ok;
npforeach_wait(S,N) ->
    receive
      S -> npforeach_wait(S,N-1)
    end.

npforeach_f(Parent, F, I) ->
    _ = (catch F(I)),
    Parent ! Parent.

%% pforeach without order constraint %%
%% Author : Saravanan Vijayakumaran

pforeach_orderless(F, L) ->
    S = self(),
    Ref = make_ref(),
    Pids = pmap(fun(I) -> spawn(fun() -> pforeach_f_orderless(S,F,I,Ref) end) end, L),
    pforeach_wait_orderless(length(Pids), Ref).

pforeach_wait_orderless(N, Ref) when N > 0 ->
    receive
        Ref -> pforeach_wait_orderless(N-1, Ref)
    end;
pforeach_wait_orderless(0, _Ref) -> ok.

pforeach_f_orderless(Parent, F, I, Ref) ->
    _ = (catch F(I)),
    Parent ! Ref.

