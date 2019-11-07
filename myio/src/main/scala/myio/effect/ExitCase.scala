package myio.effect

sealed abstract class ExitCase[+E] extends Product with Serializable

object ExitCase {

  // An [[ExitCase]] that signals successful completion.
  final case object Completed extends ExitCase[Nothing]

  // An [[ExitCase]] signaling completion in failure.
  final case class Error[+E](e: E) extends ExitCase[E]

  // An [[ExitCase]] signaling that the action was aborted.
  final case object Canceled extends ExitCase[Nothing]

  def complete[E]: ExitCase[E]    = Completed
  def error[E](e: E): ExitCase[E] = Error[E](e)
  def canceled[E]: ExitCase[E]    = Canceled

  // Converts from Scala's `Either`, which is often the result of `MonadError`'s `attempt` operation, into an ExitCase.
  def attempt[E, A](value: Either[E, A]): ExitCase[E] =
    value match {
      case Left(e)  => ExitCase.error(e)
      case Right(_) => ExitCase.complete
    }
}
