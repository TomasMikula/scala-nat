/** Exists type `X` such that there is a value of type `F[X]`. */
sealed trait Exists[F[_]]:
  type X
  def value: F[X]

object Exists:
  case class Some[F[_], A](value: F[A]) extends Exists[F]:
    override type X = A

  def apply[F[_], A](value: F[A]): Exists[F] =
    Some(value)

  def apply[A]: ExistsArg[A] =
    ExistsArg[A]

  class ExistsArg[A]:
    def suchThat[F[_]](fa: F[A]): Exists[F] =
      Some(fa)

