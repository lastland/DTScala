This is a demonstration of how to implement dependently-typed
functions in Scala.

[This tutorial](https://lastland.github.io/pl-blog/posts/2017-09-03-dtscala.html) is the best place to get started.

For more details, refer to the code:

`Nat.scala` demonstrates how to define singleton types of natural
numbers in Scala.

`GADT.scala` shows how to use subtyping to model inductive data types
(like Haskell's generic algebraic data types, a.k.a GADT).

`Vec.scala` demonstrates how to use singleton types of natural numbers
to encode a vector whose length information is in its type.

`RBT.scala` shows how to implement a dependently-typed red-black tree.
