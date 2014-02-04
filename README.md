<img src="http://zv.github.io/images/artifact.png">

# Artifact

## Introduction

Artifact is a distributed storage system inspired by the Amazon [Dynamo
Paper](http://www.cs.ucsb.edu/~agrawal/fall2009/dynamo.pdf). Artifact began 
life as an implementation of the dynamo paper in Erlang. The introduction of
Elixir brought the BEAM/OTP ecosystem forward significantly. As a consequence
the project today is a completely, from the ground up rewrite of Artifact in
Elixir and has strayed considerably from an idealized Dynamo system.

## What does it do?

### Not Suitable for Production
Artifact is intended to explore concepts and emerging useful patterns in
database design and distributed systems. It is not intended as a replacement for
any commercial database.

### The Pitch

Artifact is a key-value store that attempts to keep all data in RAM at all
times. (Despite sharing a protocol with memcached, it is not intended as a
cache). Artifact ultimately seeks to target users on a 10GB Ethernet LAN.

### Features

* No single point of failure
* Recovers very quickly from server crashs (1-2 seconds)
* Flexible storage backends
* Automatic Load Balacing
* Low Latency
* Already compatible with any language that has a memcache client
* Fast "eventual" consistency 
* [Easy inconsistency reconciliation](http://research.microsoft.com/en-us/um/people/lamport/pubs/time-clocks.pdf)

## Run

```
$ git clone https://github.com/zv/Artifact.git
$ mix compile
$ iex -S mix
```
