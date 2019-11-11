package littleMonadDemo

import littleMonadDemo.monad._

package object id {

  type Id[A] = A

  implicit val idMonad: Monad[Id] = new Monad[Id] {

    override def pure[A](a: A): Id[A] =
      a

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
      f(fa)
  }
}
