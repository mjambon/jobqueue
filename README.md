jobqueue
==

This is an OCaml library for detaching CPU-intensive jobs into their
own process. The maximum number of jobs running in parallel is
configurable.

Properties that may not exist in other libraries include:

* ability to continuously submit new jobs
* no need for the input data to be serializable

Implementation
--

A process is created by a call to `fork()`, allowing the child process
to inherit all the data it needs from its parent without
complications. Only the result of its execution is serialized, which
is done with the `Marshal` module, and passed to the parent process
using a pipe.

Concurrency is managed with the `Lwt` library.

Due to the reliance on `fork()`, this library won't work on Windows.

Authors
--

The original Jobqueue module was written by Martin Jambon at Esper
and released to the public in 2017.
