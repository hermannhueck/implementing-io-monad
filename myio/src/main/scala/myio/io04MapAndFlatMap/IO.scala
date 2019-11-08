/*
  In step 4 we add map, flatMap and flatten to the IO case class.
  This allows us to compose small IO instances into a bigger program.
  Composition can easily be done in a for-comprehension.
 */

package myio.io04MapAndFlatMap

case class IO[A](unsafeRun: () => A) {

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())

  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))

  def flatten[B](implicit ev: A <:< IO[B]): IO[B] =
    flatMap(a => a)
}
