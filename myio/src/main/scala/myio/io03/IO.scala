/*
  In step 3 the Function0[A] is named 'run' and wrapped in a case class.
  To run the program we must unwrap the 'run' function and invoke it.
 */

package myio.io03

// IO[A] wraps a Function0[A]
//
case class IO[A](unsafeRun: () => A)
