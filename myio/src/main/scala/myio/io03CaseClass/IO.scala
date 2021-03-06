/*
  In step 3 the Function0[A] is named 'run' and wrapped in a case class.
  To run the program we must unwrap the 'run' function and invoke it.
 */

package myio.io03CaseClass

// IO[A] wraps a Function0[A]
//
final case class IO[A](unsafeRun: () => A)
