package dependent.vectors
import dependent.nat._

sealed trait Vec[A, N <: Nat] {
  def app[M <: Nat](b: Vec[A, M])(implicit n: N): Vec[A, n.:+[M]]
  def apply[M <: Nat](m: M)(implicit lt: Lt[M, N]): A
}

case class Nil[A]() extends Vec[A, Z.type] {
  def app[M <: Nat](b: Vec[A, M])(implicit n: Z.type) = b
  def apply[M <: Nat](m: M)(implicit lt: Lt[M, Z.type]) = ???
}

case class Cons[A, N <: Nat](h: A, t: Vec[A, N])
    extends Vec[A, S[N]] {
  def app[M <: Nat](b: Vec[A, M])(implicit n: S[N]) =
    Cons(h, t.app(b)(n.pred))
  def apply[M <: Nat](m: M)(implicit lt: Lt[M, S[N]]) = lt match {
    case LtZ() => h
    case LtS(ltp) => t.apply(m.pred)(ltp)
  }
}

object Vec {
  def rep[A, N <: Nat](n: N, x: A)(implicit f: A => Vec[A, N]):
      Vec[A, N] = f(x)

  implicit def repz[A] : A => Vec[A, Z.type] = (_: A) => Nil[A]
  implicit def repn[A, N <: Nat] (implicit f: A => Vec[A, N]) :
      A => Vec[A, S[N]] = (x: A) => Cons(x, f(x))
}
