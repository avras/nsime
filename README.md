NSIME (Network Simulator in Erlang)
===================================

NSIME is a network simulator written in the Erlang language. It is licensed
under the GNU GPL v3.  The motivation for developing NSIME is to accelerate
network simulation on multicore processors using the Erlang concurrency model.
NSIME is modeled after the [ns-3 simulator][1] with respect to the simulation
entities and their interrelationships.

Installation
------------
To use NSIME, do the following

* Install [Erlang](http://www.erlang.org)
* Install [rebar](https://github.com/basho/rebar) and add its location to the
  PATH variable
* Clone the NSIME repository and run `make`
* Run the tests using `make tests`

Features
--------
The current implementation of NSIME contains the subset of the ns-3
implementation required to run the UDP echo client/server interaction scenario
(the [first example][2] in the official ns-3 tutorial).

Code Organization
-----------------
The NSIME repository consists of five directories.

* `src`: Contains the source code of all the NSIME modules.
* `include`: Contains files defining records and types which are
  used in NSIME.
* `test`: Contains the unit tests for the modules in NSIME.
* `ebin`: Contains the object files which are generated during the
  compilation of NSIME.
* `examples`: Contains network simulation examples using Erlang.
  This directory has its own `src` and `ebin` subdirectories to keep the core
  modules in NSIME separate from the examples.

A `logs` directory is created when the tests are run. The results of the test
execution and information about the code coverage of the tests can be read by
opening `logs/index.html` in a browser.

Simulation Examples
--------------------
There are currently two simulation examples in NSIME both based on UDP
client/server interaction. They are both located in the `examples/src`
directory.

* `examples/src/ns3_tutorial_first.erl`

    The first example is an NSIME port of the [first example][2] in the
    official ns-3 tutorial. The ns-3 script can be found in
    `NS3ROOT/examples/tutorial/first.cc` where `NS3ROOT` is the root directory
    of the ns-3 source code.  It consists of a single UDP echo client sending a
    specified number of packets to a UDP echo server. The UDP echo server
    echoes each received packet back to the client. The example can be executed
    using the following command in the main NSIME directory.

    `erl -noshell +P 1000000 -pa ebin/ -pa examples/ebin/  -run
    ns3_tutorial_first start 1 -run init stop`

    The number 1 represents the number of packets sent by the UDP client.

* `examples/src/udp_cs_pairs.erl`

    The second example consists of a specified number of UDP echo client/server
    pairs exchanging packets. The corresponding ns-3 script can be found at
    [https://gist.github.com/4085165](https://gist.github.com/4085165).  Each
    client sends 10 packets to a unique server which replies to each packet.
    The example can be executed using the following command in the main NSIME
    directory.

    `erl -noshell +P 1000000 -pa ebin/ -pa examples/ebin/  -run udp_cs_pairs
    start 50 -run init stop`

    The number 50 represents the number of client/server pairs which are
    created.

[1]: http://www.nsnam.org/ "ns-3 simulator"
[2]: http://www.nsnam.org/docs/release/3.15/tutorial/singlehtml/index.html#a-first-ns-3-script

