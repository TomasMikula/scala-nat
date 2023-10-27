/** Uninhabited marker types. */
sealed trait Z
sealed trait S[N]

/** Unary representation of natural numbers. */
enum Nat[N]:
  case Zero               extends Nat[Z]
  case Succ[N](n: Nat[N]) extends Nat[S[N]]

  def toInt: Int =
    this match
      case Zero    => 0
      case Succ(n) => 1 + n.toInt

object Nat:
  def pred[N](n: Nat[S[N]]): Nat[N] =
    n match
      case Succ(m) => m

  def apply(n: Int): Nat[?] =
    require(n >= 0, s"Expected non-negative integer, got $n")
    if (n == 0) Zero else Succ(Nat(n-1))

import Nat.*

/** Witnesses that `K + L = M`. */
sealed trait Plus[K, L, M]:
  import Plus.*

  lazy val lArg  : Nat[K] = computeLArg
  lazy val rArg  : Nat[L] = computeRArg
  lazy val result: Nat[M] = computeResult

  private def computeLArg: Nat[K] =
    this match
      case PlusZero(k) => k
      case PlusSucc(p) => p.lArg

  private def computeRArg: Nat[L] =
    this match
      case PlusZero(_) => Zero
      case PlusSucc(p) => Succ(p.rArg)

  private def computeResult: Nat[M] =
    this match
      case PlusZero(k) => k
      case PlusSucc(p) => Nat.Succ(p.result)

  /** Commutativity of addition. */
  def flip: Plus[L, K, M] =
    this match
      case PlusZero(k) => Plus.leftZero(k)
      case PlusSucc(p) => succPlus(p.flip)

  /** Addition has a unique result (i.e. it is a function):
   *  if `K + L = M` and `K + L = N`, then `M = N`.
   */
  def deriveEq[N](that: Plus[K, L, N]): M =:= N

object Plus:
  case class PlusZero[M](left: Nat[M]) extends Plus[M, Z, M]:
    override def deriveEq[N](that: Plus[M, Z, N]): M =:= N =
      that match { case PlusZero(_) => summon[M =:= N] }

  case class PlusSucc[K, L, M](prev: Plus[K, L, M]) extends Plus[K, S[L], S[M]]:
    override def deriveEq[SN](that: Plus[K, S[L], SN]): S[M] =:= SN =
      that match { case PlusSucc(q) => (prev deriveEq q).liftCo[S] }

  def apply[M, N](m: Nat[M], n: Nat[N]): Exists[[S] =>> Plus[M, N, S]] =
    n match
      case Zero => 
        Exists[M] suchThat PlusZero(m)
      case Succ(p) => 
        Plus(m, p) match
          case Exists.Some(r) => Exists(PlusSucc(r))

  /** 0 is the right identity element for addition. */
  def rightZero[N](n: Nat[N]): Plus[N, Z, N] =
    PlusZero(n)

  /** 0 is the left identity element for addition. */
  def leftZero[N](n: Nat[N]): Plus[Z, N, N] =
    n match
      case Zero    => PlusZero[Z](Zero)
      case Succ(p) => PlusSucc(leftZero(p))

  /** Symmetric to the [[PlusSucc]] constructor. */
  def succPlus[K, L, M](prev: Plus[K, L, M]): Plus[S[K], L, S[M]] =
    prev match
      case PlusZero(k) => PlusZero(Succ(k))
      case PlusSucc(p) => PlusSucc(succPlus(p))

