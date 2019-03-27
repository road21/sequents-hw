package calc.lambda.systemF

import calc.lambda.systemF.Name.Name
import calc.lambda.systemF.Operation.{BinaryOp, Plus, UnaryOp}
import calc.lambda.systemF.Term._
import cats.Id
import cats.arrow.FunctionK

trait TermAlg[A] {
  def v(name: Name): A                       // variable
  def lam(name: Name, ty: Type, term: A): A  // abstraction
  def app(t1: A, t2: A): A                   // application

  def lamT(x: Name, term: A): A              // type abstraction
  def appT(t: A, ty: Type): A                // type application

  // extensions
  def tuple(ts: List[A]): A
  def proj(t: A, n: Int): A

  def boolean(v: Boolean): A
  def int(v: Int): A
  def double(v: Double): A
  def string(s: String): A

  def unaryOp(op: UnaryOp, t: A): A
  def binaryOp(op: BinaryOp, l: A, r: A): A
}

object TermAlg {
  type TermFinal = FunctionK[TermAlg, Id]

  def apply[A](implicit t: TermAlg[A]): TermAlg[A] = t

  object syntax {
    implicit class termAlgOps[A: TermAlg](t1: A) {
//      def <=(t2: A): A = TermAlg[A].beq(t1, t2)
//      def +(t2: A): A = TermAlg[A].plus(t1, t2)
    }
  }

  object instances {
    implicit val TermInstance: TermAlg[Term] = new TermAlg[Term] {
      override def v(name: Name): Term = Var(name)
      override def lam(name: Name, ty: Type, term: Term): Term = Lam(name, ty, term)
      override def app(t1: Term, t2: Term): Term = App(t1, t2)
      override def lamT(x: Name, term: Term): Term = LamT(x, term)
      override def appT(t: Term, ty: Type): Term = AppT(t, ty)
      override def tuple(ts: List[Term]): Term = TupleN(ts)
      override def proj(t: Term, n: Int): Term = Projection(t, n)
      override def boolean(v: Boolean): Term = BooleanValue(v)
      override def int(v: Int): Term = IntValue(v)
      override def double(v: Double): Term = DoubleValue(v)
      override def string(s: String): Term = ???
      override def binaryOp(op: BinaryOp, l: Term, r: Term): Term = op match {
        case Plus => PlusOp(l, r)
      }
      override def unaryOp(op: Operation.UnaryOp, t: Term): Term = ???
    }

    implicit val StringInstance: TermAlg[String] = new TermAlg[String] {
      def printName(n: Name): String = n._1 + (if (n._2 == 0) "" else n._2)

      override def v(name: Name): String = printName(name)
      override def lam(name: Name, ty: Type, term: String): String = {
//        import TypeAlg.instances._
//        implicitly[TypeAlg[String]]

        s"λ${printName(name)}: ${ty.to[String](TypeAlg.instances.StringInstance)}. $term"
      }

      override def app(t1: String, t2: String): String =
        s"($t1 $t2)"

      override def lamT(x: Name, term: String): String =
        s"λ${printName(x)}. $term"

      override def appT(t: String, ty: Type): String =
        s"($t [$ty])"

      override def tuple(ts: List[String]): String =
        "(" + ts.mkString(", ") + ")"

      override def proj(t: String, n: Int): String =
        s"$t._$n"

      override def boolean(v: Boolean): String = v.toString
      override def int(v: Int): String = v.toString
      override def double(v: Double): String = v.toString
      override def string(s: String): String = s""""$s""""

      override def binaryOp(op: BinaryOp, l: String, r: String): String = op match {
        case Plus => s"($l + $r)"
      }

      override def unaryOp(op: UnaryOp, t: String): String = ???
    }
  }
}