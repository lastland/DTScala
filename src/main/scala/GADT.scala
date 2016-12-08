package dependent.gadt

/*
 data Expr a where
     I   :: Int  -> Expr Int
     B   :: Bool -> Expr Bool
     Add :: Expr Int -> Expr Int -> Expr Int
     Mul :: Expr Int -> Expr Int -> Expr Int
     Eq  :: Expr Int -> Expr Int -> Expr Bool

 eval :: Expr a -> a
 eval (I n) = n
 eval (B b) = b
 eval (Add e1 e2) = eval e1 + eval e2
 eval (Mul e1 e2) = eval e1 * eval e2
 eval (Eq  e1 e2) = eval e1 == eval e2
 */

sealed trait Expr[A] {
  def eval: A
}

case class I(n: Int) extends Expr[Int] {
  def eval = n
}

case class B(b: Boolean) extends Expr[Boolean] {
  def eval = b
}

case class Add(a: Expr[Int], b: Expr[Int]) extends Expr[Int] {
  def eval = a.eval + b.eval
}

case class Mul(a: Expr[Int], b: Expr[Int]) extends Expr[Int] {
  def eval = a.eval / b.eval
}

case class Eq(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean] {
  def eval = a.eval == b.eval
}

/*
 data Maybe a = Nothing | Just a
 */

sealed trait MyMaybe[+A]

case object MyNothing extends MyMaybe[Nothing]

class MyJust[A] extends MyMaybe[A]
