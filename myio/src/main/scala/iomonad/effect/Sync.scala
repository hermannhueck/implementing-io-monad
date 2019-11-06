package iomonad.effect

trait Sync[F[_]] extends Bracket[F, Throwable] {

  /**
    * Suspends the evaluation of an `F` reference.
    *
    * Equivalent to `FlatMap.flatten` for pure expressions,
    * the purpose of this function is to suspend side effects
    * in `F`.
    */
  def suspend[A](thunk: => F[A]): F[A]

  /**
    * Alias for `suspend` that suspends the evaluation of
    * an `F` reference and implements `cats.Defer` typeclass.
    */
  def defer[A](thunk: => F[A]): F[A] = suspend(thunk)

  /**
    * Lifts any by-name parameter into the `F` context.
    *
    * Equivalent to `Applicative.pure` for pure expressions,
    * the purpose of this function is to suspend side effects
    * in `F`.
    */
  def delay[A](thunk: => A): F[A] = suspend(pure(thunk))
}

object Sync {

  def apply[F[_]: Sync]: Sync[F] = implicitly
}
