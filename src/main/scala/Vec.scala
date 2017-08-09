package dependent.vectors
import scala.language.higherKinds
import scala.language.implicitConversions
import dependent.nat._

sealed trait Vec[N <: Nat] {
  def app[M <: Nat](b: Vec[M])(implicit n: N): Vec[n.Plus[M]]
}

case object Nil extends Vec[Z.type] {
  def app[M <: Nat](b: Vec[M])(implicit n: Z.type) = b
}

case class Cons[N <: Nat](h: Int, t: Vec[N])
    extends Vec[Succ[N]] {
  def app[M <: Nat](b: Vec[M])(implicit n: Succ[N]) =
    Cons(h, t.app(b)(n.pred))
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
