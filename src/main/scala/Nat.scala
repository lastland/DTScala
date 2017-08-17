package dependent.nat
import scala.language.higherKinds

sealed trait Nat {
  type :+[M <: Nat] <: Nat
  def +[M <: Nat](m: M): :+[M]
}

case object Z extends Nat {
  type :+[M <: Nat] = M
  override def +[M <: Nat](m: M) = m
}

case class S[P <: Nat](pred: P) extends Nat {
  type :+[M <: Nat] = S[pred.:+[M]]
  override def +[M <: Nat](m: M) = S(pred + m)
}

object Nat {
  /* get the value from a singleton type */
  def get[N <: Nat](implicit n: N) = n

  implicit def get_z: Z.type = Z

  implicit def get_s[N <: Nat](implicit pv: N): S[N] = S(pv)
}

sealed trait Lt[N <: Nat, M <: Nat]

case class LtZ[M <: Nat]() extends Lt[Z.type, S[M]]

case class LtS[N <: Nat, M <: Nat](lt: Lt[N, M])
    extends Lt[S[N], S[M]]

object Lt {
  implicit def z_lt_s[N <: Nat] = LtZ[N]()

  implicit def s_lt_s[N <: Nat, M <: Nat](implicit lt: Lt[N, M]) =
    new LtS[N, M](lt)
}
