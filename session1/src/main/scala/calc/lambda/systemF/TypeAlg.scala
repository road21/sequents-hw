package calc.lambda.systemF

import calc.lambda.systemF.Name.Name

trait TypeAlg[A] {
  def v(name: Name): A            // type variable
  def impl(t1: A, t2: A): A        // function type
  def forall(name: Name, t: A): A // universal type

  // extensions
  def product(ts: List[A]): A
  def record(ts: List[(String, A)]): A

  def bool: A
  def int: A
  def double: A
  def string: A
}

object TypeAlg {
  def apply[A](implicit t: TypeAlg[A]): TypeAlg[A] = t

  object syntax {
    implicit class typeAlgOps[A: TypeAlg](t1: A) {
      def ->>(t2: A): A = TypeAlg[A].impl(t1, t2)
      def ×(t2: A): A = TypeAlg[A].product(List(t1, t2))
    }
  }

  object instances {
    implicit val StringInstance: TypeAlg[String] = new TypeAlg[String] {
      def printName(n: Name): String = n._1 + (if (n._2 == 0) "" else n._2)

      override def v(name: Name): String = printName(name)
      override def impl(t1: String, t2: String): String = s"($t1 -> $t2)"
      override def forall(name: Name, t: String): String = s"∀${printName(name)}. $t"
      override def record(ts: List[(String, String)]): String =
        s"(${ts.map { case (x, y) => s"$x: $y"}.mkString(", ")})"
      override def product(ts: List[String]): String = ts.mkString("×")
      override def bool: String = "Bool"
      override def int: String = "Int"
      override def double: String = "Double"
      override def string: String = "String"
    }
  }
}