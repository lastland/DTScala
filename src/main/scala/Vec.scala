package dependent.vectors
import scala.language.higherKinds
import scala.language.implicitConversions
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

object Rep {
  sealed trait VecFun[N <: Nat] extends Function1[Int, Vec[N]]

  implicit def repz : VecFun[Z.type] = new VecFun[Z.type] {
    def apply(x: Int) = Nil
  }

  implicit def repn[N <: Nat] (implicit pv: VecFun[N]) :
      VecFun[Succ[N]] = new VecFun[Succ[N]] {
    def apply(x: Int) = Cons(x, pv.apply(x))
  }

  def rep[N <: Nat](n: N, x: Int)(implicit v: VecFun[N]): Vec[N] =
    v.apply(x)
}
