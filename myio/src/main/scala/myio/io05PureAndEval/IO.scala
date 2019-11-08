/*
  Step 5 adds the IO companion object, which provides 'pure' and 'eval'.
  With 'pure' we can lift a pure value of type A into the IO context.
  'pure' is eager, i.e. executed immediately.
  'eval' takes a thunk (a block of computation) and turns it into a Function0[A] and wraps it into IO.
  'eval' is lazy.

  This allows us to simplify the for-comprehension in our program a bit.
  We no longer need to specify functions in order to create instances of IO.
 */

package myio.io05PureAndEval

final case class IO[A](unsafeRun: () => A) {

  import IO._

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())

  def map[B](f: A => B): IO[B] =
    flatMap(a => pure(f(a)))

  def flatten[B](implicit ev: A <:< IO[B]): IO[B] =
    flatMap(a => a)
}

object IO {

  def pure[A](value: A): IO[A] =
    IO(() => value)

  def eval[A](thunk: => A): IO[A] =
    IO(() => thunk)
}
