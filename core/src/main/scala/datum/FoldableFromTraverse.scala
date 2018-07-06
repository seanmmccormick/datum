package datum

import cats.data.Const
import cats.syntax.traverse
import cats.{Eval, Monoid, Traverse}

// https://gist.github.com/Baccata/0b972953cd65421d6a475b37aadad65b

// implements foldLeft/foldRight in terms of traverse
abstract class FoldableFromTraverse[F[_]] extends Traverse[F] {

  override def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B =
    traverse[Const[B, ?], A, B](fa)(a => Const(f(a))).getConst

  private def andThenMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def combine(f: A => A, g: A => A) = f andThen g
    def empty: A => A                 = (a: A) => a
  }

  private def composeMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def combine(f: A => A, g: A => A) = f compose g
    def empty: A => A                 = (a: A) => a
  }

  private def defer[B](f: Eval[B] => Eval[B]): Eval[B] => Eval[B] = evalB => Eval.defer(f(evalB))

  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    foldMap(fa)(f.curried andThen defer)(composeMonoid).apply(lb)
  }

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    foldMap[A, B => B](fa){a => b => f(b, a)}(andThenMonoid[B]).apply(b)

}
