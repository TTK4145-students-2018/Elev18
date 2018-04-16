# Elev18
## Getting there

A scalable elevator control system written in erlang.

### Starting elevators
1. On every computer (or ssh-terminal), run 
$bash compile.sh
2. In a new terminal, run
$ElevatorServer
once for every node you wish to create.
3. To start a node, go to the erlang terminal
the bash script started, and run
$node_center:start().

### Externally written code/modules
The only pre-made module we're using is driver.erl, which
was provided in the TTK4145-repo and intended as a way of
interfacing with the elevators.