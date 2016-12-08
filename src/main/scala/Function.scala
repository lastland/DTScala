package dependent.function

trait Function[-A, +B] {
  def apply(x: A): B
}

object Example {
  val incr = new Function[Int, Int] {
    def apply(x: Int) = x + 1
  }

  val plus = new Function[Int, Function[Int, Int]] {
    def apply(x: Int) = new Function[Int, Int] {
      def apply(y: Int) = x + y
    }
  }
}
