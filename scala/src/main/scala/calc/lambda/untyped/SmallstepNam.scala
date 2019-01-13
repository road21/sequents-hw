package calc.lambda.untyped

import calc.lambda.untyped.TermNam.Name
import Terms._
import cats.implicits._
import cats.kernel.Order

object SmallstepNam {
  def fresh(names: List[Name]): Name = {
    names match {
      case Nil => X
      case x :: xs =>
        val (n, v) = xs.foldLeft(x)(Order[Name].max)
        (n, v + 1)
    }
  }

  def freeVars(term: Term): Set[Name] = {
    term.fold(new TermAlg[Set[Name]] {
      def vari(n: Name): Set[Name] = Set(n)
      def lam(n: Name, t: Set[Name]): Set[Name] = t.filter(_ != n)
      def app(t1: Set[Name], t2: Set[Name]): Set[Name] = t1 ++ t2
    })
  }

  def allVars(term: Term): Set[Name] = {
    term.fold(new TermAlg[Set[Name]] {
      def vari(n: Name): Set[Name] = Set(n)
      def lam(n: Name, t: Set[Name]): Set[Name] = t
      def app(t1: Set[Name], t2: Set[Name]): Set[Name] = t1 ++ t2
    })
  }
}
