                   Examples for Erigone

Example              File             Mode     Result

Arrays               array            -r       20 20 ... 20/9 8 ... 0

Concurrent counting  simple           -s       assert false
Concurrent counting  count            -s -e700 assert false

Finite automaton     fa               -s       assert false

Satisfiability       sat              -s       assert false
Satisfiability       sat3             -s       success

Second attempt       second-assert    -s       assert false
Third attempt        third-assert     -s       invalid end state
Dekker's algorithm   dekker-assert    -s       success
Weak semaphore       sem-assert       -s       success

Second attempt       second-mutex     -s -t    never claim terminated
Second attempt       second-ltl       -s -t    never claim terminated
Dekker's algorithm   dekker-mutex     -s -t    success
Weak semaphore       sem-mutex        -s -t    success

The two examples for the second attempt use different ways of expressing
that mutual exclusion is not violated.

Fourth attempt       fouth-live       -a -t    acceptance cycle
Fourth attempt       fouth-live       -f -t    acceptance cycle
Dekker's algorithm   dekker-live1     -f -t    success
Dekker's algorithm   dekker-live1     -a -t    acceptance cycle
Dekker's algorithm   dekker-live1     -f -t    success
Dekker's algorithm   dekker-live2     -a -t    acceptance cycle
Dekker's algorithm   dekker-live2     -f -t    success

The two versions of Dekker's algorithm differ in the complexity of the
LTL formula used to verify liveness.

Fairness             fair1            -a -t    acceptance cycle
Fairness             fair1            -f -t    success
Fairness             fair2            -a -t    acceptance cycle
Fairness             fair2            -f -t    acceptance cycle

The examples fair3 and fair4 give the same results as fair1 and fair2
and are intended to check the addition of null transitions to blocked
processes.

Channels: FIFO       ch-fifo          -r       1,2,3/4,5,6/7,8,9/10,11,12
Channels: Sorted     ch-sorted        -r       4,5,6/7,8,8/7,8,9/1,2,3/20,21,22
Channels: Match      ch-match         -r       1,2,3/5,6/7,8/invalid end state
Channels: Random     ch-random        -r       5,6/10,11/8
Channels: Poll       ch-poll          -r       1,_,_/_,8,_
Channels: Expression ch-expr          -r       obvious ...

Mtype                mtype            -f       ack reply/request/ack 10 11
                                               reply 8 9/request 2 3request 2 3
                                               reply 4 5/ack 6 7
