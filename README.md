<img src="http://zv.github.io/images/artifact.png">

# Artifact
**I began rewriting Artifact in Elixir some time ago and it is not a complete
port of the Erlang code -- continue using Artifact in Erlang if you want
something running**

## Introduction

Artifact is a distributed storage system inspired by the Amazon
[Dynamo Paper](http://www.cs.ucsb.edu/~agrawal/fall2009/dynamo.pdf). The
introduction of [Elixir](http://elixir-lang.org) precipitated numerous changes
in the OTP ecosystem, consequently the project began anew as a ground up rewrite
Elixir and has introduced several features that cause it to diverge
significantly from it's initial implementation.

Artifact is a super-high-speed distributed key value store, designed for
applications with a large number of servers in a datacenter than need
low-latency reads.


### Entirely Nonvolatile:
Artifact stores it's entire dataset in volatile storage, aggregated across
multiple machines, because it recovers from server crashes very fast (about
1-2 seconds) and so availability gaps are unnoticable and durability is
ensured as long as n/2 + 1 machines stay online. (The data itself may be
made available through a loop filesystem, and can be written to DETS or some
other nonvolatile storage as well.)

### Low Latency
Most writes happen in less than 50μs, most reads in less than 25μs.

### Consistency
Artifact provides an slightly stronger consistency than dynamo -- all
updates in Artifact are consistent, immediately visible, and durable.


## What does it do?

### Not Suitable for Production
Artifact is intended to explore concepts and emerging useful patterns in
database design and distributed systems. It is not intended as a replacement for
any commercial database.
