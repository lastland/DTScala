package dependent.vectors
import dependent.nat._

sealed trait Vec[N <: Nat] {
  def app[M <: Nat](b: Vec[M])(implicit n: N): Vec[n.Plus[M]]
  def apply[M <: Nat](m: M)(implicit lt: Lt[M, N]): Int
}

case object Nil extends Vec[Z.type] {
  def app[M <: Nat](b: Vec[M])(implicit n: Z.type) = b
  def apply[M <: Nat](m: M)(implicit lt: Lt[M, Z.type]) = 0
}

case class Cons[N <: Nat](h: Int, t: Vec[N])
    extends Vec[Succ[N]] {
  def app[M <: Nat](b: Vec[M])(implicit n: Succ[N]) =
    Cons(h, t.app(b)(n.pred))
  def apply[M <: Nat](m: M)(implicit lt: Lt[M, Succ[N]]) = lt match {
    case LtZ() => h
    case LtS(ltp) => t.apply(m.pred)(ltp)
  }
}

object Vec {
  implicit val repz : Int => Vec[Z.type] = (_: Int) => Nil

  implicit def repn[N <: Nat] (implicit f: Int => Vec[N]) :
      Int => Vec[Succ[N]] = (x: Int) => Cons(x, f(x))

  def rep[N <: Nat](n: N, x: Int)(implicit f: Int => Vec[N]): Vec[N] =
    f(x)
}
