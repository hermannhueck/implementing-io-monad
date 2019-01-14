package iomonad.effect

import scala.language.higherKinds

trait ConcurrentEffect[F[_]] extends Concurrent[F] {

  // def runCancelable[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[F]]
}
