package datum

object helpers {

  // probably ought to be in the
  type Algebra[F[_], A] = F[A] => A

  type Coalgebra[F[_], A] = A => F[A]
}
