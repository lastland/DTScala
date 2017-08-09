package dependent.nat
import scala.language.higherKinds

sealed trait Nat {
  type Plus[M <: Nat] <: Nat
  def +[M <: Nat](m: M): Plus[M]
}

case object Z extends Nat {
  type Plus[M <: Nat] = M
  override def +[M <: Nat](m: M) = m
}

case class Succ[P <: Nat](pred: P) extends Nat {
  type Plus[M <: Nat] = Succ[pred.Plus[M]]
  override def +[M <: Nat](m: M) = Succ(pred + m)
}

object Nat {
  /* get the value from a singleton type */
  def get[N <: Nat](implicit n: N) = n

  implicit def zVal: Z.type = Z

  implicit def sVal[N <: Nat](implicit pv: N): Succ[N] = Succ(pv)
}
