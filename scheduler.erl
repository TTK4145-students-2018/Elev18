-module(scheduler).
-export([]).

% Calculates cost, and determines whether it is optimal 
% for this elevator to perform a task instead of one of the
% others. Keeps track of worldview* and own state (from fsm) to
% serve as a basis of calculations. Needs the same worldview as
% the other schedulers, to make sure every scheduler reaches
% decisions based on correct information about the entire system. 

% TODO:
% 

evaluate(PID, )