package iomonad.effect

import scala.language.higherKinds

trait Fiber[F[_], A] {
  /**
    * Triggers the cancellation of the fiber.
    *
    * Returns a new task that will trigger the cancellation upon
    * evaluation. Depending on the implementation, this task might
    * await for all registered finalizers to finish, but this behavior
    * is implementation dependent.
    *
    * Note that if the background process that's evaluating the result
    * of the underlying fiber is already complete, then there's nothing
    * to cancel.
    */
  def cancel: CancelToken[F]

  /**
    * Returns a new task that will await for the completion of the
    * underlying fiber, (asynchronously) blocking the current run-loop
    * until that result is available.
    */
  def join: F[A]
}
