package calc.lambda.systemF

import calc.lambda.systemF.Name.Name

trait TypeAlg[A] {
  def v(name: Name): A            // type variable
  def impl(t1: A, t2: A): A        // function type
  def forall(name: Name, t: A): A // universal type

  // extensions
  def product(t1: A, t2: A): A

  def unit: A
  def bool: A
  def int: A
}

object TypeAlg {
  def apply[A](implicit t: TypeAlg[A]): TypeAlg[A] = t

  object syntax {
    implicit class typeAlgOps[A: TypeAlg](t1: A) {
      def ->>(t2: A): A = TypeAlg[A].impl(t1, t2)
      def ×(t2: A): A = TypeAlg[A].product(t1, t2)
    }
  }
}