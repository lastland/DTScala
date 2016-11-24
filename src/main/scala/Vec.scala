package dependent.vectors
import scala.language.higherKinds
import dependent.nat._

sealed trait Vec[N <: Nat] {
  type NM[M <: Nat] <: Nat
  type T[M <: Nat] = Vec[NM[M]]
  def app[M <: Nat](b: Vec[M]): T[M]
}

case object Nil extends Vec[Z.type] {
  type NM[M <: Nat] = M
  def app[M <: Nat](b: Vec[M]) = b
}

case class Cons[N <: Nat](h: Int, t: Vec[N])
    extends Vec[Succ[N]] {
  type NM[M <: Nat] = Succ[t.NM[M]]
  def app[M <: Nat](b: Vec[M]) = Cons(h, t.app(b))
}
