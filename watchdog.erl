-module(watchdog).
-export([kill/2, start/2, start_handler/0]).

% the watchdog timer is to have control over
% the completion of individual processes, which
% can spawn and kill watchdogs as a safety net
% that monitors the process

watchdog(WDHandler, Data, Timeout) ->
	receive
		{kill, Data} ->
			WDPID = self(),
			WDHandler ! {WDPID, dead},
			io:format("wd_watchdog: watchdog died: ~p~n", [self(), Data]),
			exit(self(), normal) %exit(PID, reason)
	after
		Timeout ->
			WDPID = self(),
			WDHandler ! {WDPID, kick, Data},
			io:format("wd_watchdog: timed out, kicking the dog: ~p~n", [self(), Data]),
			exit(self(), normal)
	end.


watchdog_handler(Watchdogs) ->
	io:format("wd_handler: existing watchdogs: ~p~n", [Watchdogs]),
	receive
		{kill, Data} ->
			lists:foreach(fun(WDPID) -> WDPID ! {kill, Data} end, Watchdogs),
			watchdog_handler(Watchdogs);
		{kick, Data} ->
			DogeHandler = self(),
			FreshWD = spawn(fun() -> watchdog(DogeHandler, Data, 10000) end), %10000ms = 10s
			io:format("wd_handler: fresh dog: ~p~n", [self(), Data]),
			watchdog_handler([FreshWD|Watchdogs]);

		{WDPID, kick, Data} ->
			WDUpdated = lists:delete(WDPID, Watchdogs), %lists:delete(Elem, List)
			watchdog_handler(WDPID);
		{WDPID, dead} ->
			WDUpdated = lists:delete(WDPID, Watchdogs),
			watchdog_handler(WDUpdated);

		Other ->
			io:format("wd_handler: don't know what this means: ~p~n", [Other]),
			watchdog_handler(Watchdogs)
	end.

kill(WDHandler, Data) ->
	WDHandler ! {kill, Data}.

start(WDHandler, Data) ->
	io:format("wd: releasing the hounds"),
	WDHandler ! {kick, Data}.

start_handler() ->
	spawn(fun() -> watchdog_handler([]) end).