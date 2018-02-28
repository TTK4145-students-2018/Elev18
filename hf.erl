-module(hf).
-export([len/1, fac/2, flip/1]).

len([]) -> 0;
len([_|L]) -> 1 + len(L). %returns length of a list


fac(0,A) when A /= 0 -> 0; %0^A = 0
fac(N,0) -> 1; %N^0 = 1
fac(N,A) when A > 0 -> N*fac(N,A-1); %returns N^A
fac(N,A) when A < 0 -> 1/fac(N,(-1)*A).


flip([]) -> [];
flip([H|T]) -> [lists:last([H|T])] ++ flip(lists:droplast([H|T])).

