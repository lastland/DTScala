package dependent.common
import dependent.nat._

object Nat {
  lazy val zero = Z
  lazy val one = S(zero)
  lazy val two = S(one)
  lazy val three = S(two)
  lazy val four = S(three)
  lazy val five = S(four)
}
