package dependent.examples.vectors
import dependent.common.Nat._
import dependent.vectors._

object VecExample {
  val l1 = Vec.rep(three, 1)
  val l2 = Cons(2, l1)
  val a = l2(zero)
  val b = l2(one)
  val c = l2(three)
  // uncomment the following line will get a type error!
  // val d = l2(four)
}
