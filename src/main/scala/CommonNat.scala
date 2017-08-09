package dependent.common
import dependent.nat._

object Nat {
  lazy val zero = Z
  lazy val one = Succ(zero)
  lazy val two = Succ(one)
  lazy val three = Succ(two)
  lazy val four = Succ(three)
  lazy val five = Succ(four)
}
