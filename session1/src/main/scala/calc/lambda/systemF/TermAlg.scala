package calc.lambda.systemF

import calc.lambda.systemF.Name.Name
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

  def `true`: A
  def `false`: A
  def ifElse(c: A, f: A, s: A): A

  def int(v: Int): A
  def double(v: Double): A

  def plus(t1: A, t2: A): A
  def uMinus(t: A): A
  def beq(l: A, r: A): A
  def mult(t1: A, t2: A): A
}

object TermAlg {
  type Expression = FunctionK[TermAlg, Id]

  def apply[A](implicit t: TermAlg[A]): TermAlg[A] = t

  object syntax {
    implicit class termAlgOps[A: TermAlg](t1: A) {
      def <=(t2: A): A = TermAlg[A].beq(t1, t2)
      def +(t2: A): A = TermAlg[A].plus(t1, t2)
    }
  }
}