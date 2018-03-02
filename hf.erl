-module(hf). %"helper functions"
-export([fac/2, flip/1]).

% simple helper functions that can be used
% by all modules

%TODO:

fac(0,A) when A /= 0 -> 0; %0^A = 0
fac(_,0) -> 1; %N^0 = 1
fac(N,A) when A > 0 -> N*fac(N,A-1); %returns N^A
fac(N,A) when A < 0 -> 1/fac(N,(-1)*A).


flip([]) -> [];
flip([H|T]) -> [lists:last([H|T])] ++ flip(lists:droplast([H|T])).

