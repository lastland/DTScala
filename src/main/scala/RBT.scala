package dependent.rbt
import scala.language.higherKinds
import scala.language.existentials
import scala.language.implicitConversions
import dependent.nat._

sealed trait Color {
  type Incr[N <: Nat]
}

sealed trait AlmostTree[N <: Nat] {
  def blacken: RBT
  def balanceLB(z: Int, d: Tree[Color, N]): HiddenTree[Succ[N]]
  def balanceRB(a: Tree[Color, N], x: Int): HiddenTree[Succ[N]]
}

case object Red extends Color {
  type Incr[N <: Nat] = N
  case class AT[N <: Nat]
    (l: Tree[Color, N], v: Int, r: Tree[Color, N])
      extends AlmostTree[Incr[N]] {
    def blacken = RBT(BlackNode(l, v, r))
    def balanceLB(z: Int, d: Tree[Color, N]) = (l, v, r) match {
      case (RedNode(a, x, b), y, c) =>
        HR(RedNode(BlackNode(a, x, b), y, BlackNode(c, z, d)))
      case (a, x, RedNode(b, y, c)) =>
        HR(RedNode(BlackNode(a, x, b), y, BlackNode(c, z, d)))
      case (a: TreeB[N], x, b: TreeB[N]) =>
        HB(BlackNode(RedNode(a, x, b), v, r))
    }
    def balanceRB(a: Tree[Color, N], x: Int) = (l, v, r) match {
      case (RedNode(b, y, c), z, d) =>
        HR(RedNode(BlackNode(a, x, b), y, BlackNode(c, z, d)))
      case (b, y, RedNode(c, z, d)) =>
        HR(RedNode(BlackNode(a, x, b), y, BlackNode(c, z, d)))
      case (b: TreeB[N], y, c: TreeB[N]) =>
        HB(BlackNode(a, x, RedNode(b, y, c)))
    }
  }
}

case object Black extends Color {
  type Incr[N <: Nat] = Succ[N]
  case class AT[N <: Nat]
    (l: Tree[Color, N], v: Int, r: Tree[Color, N])
      extends AlmostTree[Incr[N]] {
    def blacken = RBT(BlackNode(l, v, r))
    def balanceLB(z: Int, d: Tree[Color, Incr[N]]) =
      HB(BlackNode(BlackNode(l, v, r), z, d))
    def balanceRB(a: Tree[Color, Incr[N]], x: Int) =
      HB(BlackNode(a, x, BlackNode(l, v, r)))
  }
}

sealed trait Tree[+C <: Color, N <: Nat] {
  def rev: Tree[C, N]
  def max: Option[Int]
  def maxDefault(d: Int): Int
  def insAny(x: Int): AlmostTree[N]
}

sealed trait TreeB[N <: Nat] extends Tree[Black.type, N] {
  def insBlack(x: Int): HiddenTree[N]
  def insAny(x: Int) = insBlack(x).forget
}

case object Empty extends TreeB[Z.type] {
  def rev = Empty
  def max = None
  def maxDefault(d: Int) = d
  def insBlack(x: Int) = HR(RedNode(Empty, x, Empty))
}

case class RedNode[N <: Nat]
  (l: Tree[Black.type, N], v: Int, r: Tree[Black.type, N])
    extends Tree[Red.type, N] {
  def rev = RedNode(r.rev, v, l.rev)
  def max = Some(r.maxDefault(v))
  def maxDefault(d: Int) = r.maxDefault(v)
  def insAny(x: Int) =
    if (x < v) {
      l.insBlack(x).balanceLR(v, r)
    } else if (x > v) {
      r.insBlack(x).balanceRR(l, v)
    } else {
      Red.AT[N](l, v, r)
    }
}

case class BlackNode[N <: Nat]
  (l: Tree[Color, N], v: Int, r: Tree[Color, N])
    extends TreeB[Succ[N]] {
  def rev = BlackNode(r.rev, v, l.rev)
  def max = Some(r.maxDefault(v))
  def maxDefault(d: Int) = r.maxDefault(v)
  def insBlack(x: Int) =
    if (x < v) {
      l.insAny(x).balanceLB(v, r)
    } else if (x > v) {
      r.insAny(x).balanceRB(l, v)
    } else {
      HB(BlackNode(l, v, r))
    }
}

sealed trait HiddenTree[N <: Nat] {
  def balanceLR(x: Int, r: Tree[Color, N]): AlmostTree[N]
  def balanceRR(l: Tree[Color, N], x: Int): AlmostTree[N]
  def forget: AlmostTree[N]
}

case class HR[N <: Nat](t: RedNode[N]) extends HiddenTree[N] {
  def balanceLR(x: Int, r: Tree[Color, N]) =
    Red.AT[N](t, x, r)
  def balanceRR(l: Tree[Color, N], x: Int) =
    Red.AT[N](l, x, t)
  def forget = Red.AT[N](t.l, t.v, t.r)
}

case class HB[N <: Nat](t: BlackNode[N]) extends HiddenTree[Succ[N]] {
  def balanceLR(x: Int, r: Tree[Color, Succ[N]]) =
    Red.AT[Succ[N]](t, x, r)
  def balanceRR(l: Tree[Color, Succ[N]], x: Int) =
    Red.AT[Succ[N]](l, x, t)
  def forget = Black.AT[N](t.l, t.v, t.r)
}

case class RBT(root: Tree[Black.type, _ <: Nat]) {
  def insert(x: Int) = root.insAny(x).blacken
}

object Tree {
  implicit def blackTreeIsNode[N <: Nat]
    (t: Tree[Black.type, N]): TreeB[N] =
    t match {
      case tb: TreeB[_] => tb
    }
}

object Example {
  val r0 = RBT(Empty).insert(3)
  val r1 = r0.insert(1)
  val r2 = r1.insert(5)
}
