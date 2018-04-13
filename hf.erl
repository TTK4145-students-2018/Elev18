-module(hf). %"helper functions"
-export([fac/2, flip/1, list_insert/3]).

% simple helper functions that can be used
% by all modules

%TODO:

fac(0,A) when A /= 0 -> 0; %0^A = 0
fac(_,0) -> 1; %N^0 = 1
fac(N,A) when A > 0 -> N*fac(N,A-1); %returns N^A
fac(N,A) when A < 0 -> 1/fac(N,(-1)*A).


flip([]) -> [];
flip([H|T]) -> [lists:last([H|T])] ++ flip(lists:droplast([H|T])).

list_insert(List, Element, Position) ->
  lists:sublist(List, Position - 1) 
         ++ [Element] 
         ++ lists:nthtail(Position - 1, List).

list_replace()

% Formats IP from a tuple to a string.
format_IP(IPlist) ->
	[_Head | IP] = lists:flatmap(fun(X) -> ['.', X] end, IPlist),
	lists:concat(IP).