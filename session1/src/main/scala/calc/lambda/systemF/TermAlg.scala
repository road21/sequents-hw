package calc.lambda.systemF

import calc.lambda.systemF.Name.Name
import calc.lambda.systemF.Operation.{Append, BinaryOp, Plus, UnaryOp}
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

  def record(ts: List[(String, A)]): A
  def field(t: A, n: String): A

  def list(ty: Type, v: List[A]): A
  def fold(l: A, e: A, f: A): A

  def boolean(v: Boolean): A
  def int(v: Int): A
  def double(v: Double): A
  def string(s: String): A

  def unaryOp(op: UnaryOp, t: A): A // TODO: to think about operations, fold and projections are ops too
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
        case Append => AppendOp(l, r)
      }
      override def unaryOp(op: Operation.UnaryOp, t: Term): Term = ???
      override def record(ts: List[(String, Term)]): Term = Record(ts)
      override def field(t: Term, n: String): Term = Field(t, n)
      override def list(ty: Type, v: List[Term]): Term = ListT(ty, v)
      override def fold(l: Term, e: Term, f: Term): Term = Fold(l, e, f)
    }

    implicit val StringInstance: TermAlg[String] = new TermAlg[String] {
      import TypeAlg.instances._

      def printName(n: Name): String = n._1 + (if (n._2 == 0) "" else n._2)

      override def v(name: Name): String = printName(name)
      override def lam(name: Name, ty: Type, term: String): String = {
        // TODO: why implicit not found?
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

      override def record(s: List[(String, String)]): String =
        s"(${s.map { case (x, y) => s"$x:$y" }.mkString(", ")})"

      override def field(t: String, n: String): String =
        s"$t.$n"

      override def boolean(v: Boolean): String = v.toString
      override def int(v: Int): String = v.toString
      override def double(v: Double): String = v.toString
      override def string(s: String): String = s""""$s""""

      override def binaryOp(op: BinaryOp, l: String, r: String): String = op match {
        case Plus => s"($l + $r)"
        case Append => s"($l ++ $r)"
      }

      override def unaryOp(op: UnaryOp, t: String): String = ???

      override def list(ty: Type, v: List[String]): String =
        s"List[${ty.to[String](TypeAlg.instances.StringInstance)}](${v.mkString(", ")})"

      override def fold(l: String, e: String, f: String): String = s"fold($l, $e, $f)"
    }
  }
}