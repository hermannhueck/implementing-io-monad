package iomonad

import scala.language.higherKinds

package object effect {

  type CancelToken[F[_]] = F[Unit]
}
