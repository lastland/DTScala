package dependent.nat

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
