# Implementing the IO Monad

In this repo I provide my own implementation of the IO Monad and a presentation on this topic.

My implementation of the IO Monad is just a feasibility study, not production code.

When coding this impl I was very much inspired by Monix *Task* which I studied at that time.

The API of my IO is very similar to the basics of Monix *Task*. The implementation also helped me
to understand the IO Monad and Monix *Task* (which itsself is an impl of the IO Monad).

Interop with *Future* is also supported. You can convert *IO* to a *Future*.
Vise versa you can convert a *Future* to an *IO*.

The development of my impl can be followed step by step in the files in package *iomonad*.
