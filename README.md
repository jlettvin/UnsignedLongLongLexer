UnsignedLongLongLexer
=====================

A C++ module for ultrafast LR1 atoi type conversion
with error-detection using computed goto.
Typically, this code translates faster
than data can be read/written from disk.

This module exposes a difference between
gnu C++ and MSVC C++ in that
gnu implements a computed goto and
MSVC does not,
and alternative coding styles are required
to get similar performance from
the two compilers.

This code implements full unit tests
including edge and corner cases
including integer underflow and overflow
when it is compiled as an autonomous main program.
