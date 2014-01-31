
One of Artifact's central goals is high availability. It was built as a multi-node
system in which any node is capable of receiving requests without requiring
that each node participate in each request. In a system like this, it's
important to be able to keep track of which version of a value is the most
current. This is where vector clocks come in.

When a value is stored in Artifact, it is tagged with a vector clock, establishing
its initial version. They are non-human-readable and look something like this:

    a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=

For each update, the vector clock is extended in such a way that Artifact can later
compare two versioned replicas of the object and determine the following:

* Whether one object is a direct descendant of the other
* Whether the objects are direct descendants of a common parent
* Whether the objects are unrelated in recent heritage

Using this knowledge, Artifact can auto-repair out-of-sync data when feasible or at
least provide a client with an opportunity to reconcile divergent changesets in
an application-specific manner.


Artifact uses Riak's very well written vector clock library for accomplishing this.
