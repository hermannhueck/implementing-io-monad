# Implementing the IO Monad

In this repo I provide my own implementation of the IO Monad and a presentation on this topic.

When coding this IO impl I was very much inspired by *cats.effect.IO* and *monix.eval.Task*
which I studied at that time. Both are implementions of the IO Monad.

The API of my IO is very similar to the basics of Monix *Task*. This IO implementation also helped me
to understand the IO Monad (of *cats-effect*) and Monix *Task*.

Interop with *Future* is also supported. You can convert *IO* to a *Future*.
Vice versa you can convert a *Future* to an *IO*.

The development of my impl can be followed step by step in the files in package *iomonad*.
