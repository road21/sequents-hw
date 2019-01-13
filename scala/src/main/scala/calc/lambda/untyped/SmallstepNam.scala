package calc.lambda.untyped

import calc.lambda.untyped.TermNam.Name
import Terms._
import cats.implicits._
import cats.kernel.Order

object SmallstepNam {
  def fresh(names: List[Name]): Name =
    names match {
      case Nil => X
      case x :: xs =>
        val (n, v) = xs.foldLeft(x)(Order[Name].max)
        (n, v + 1)
    }

  def freeVars(term: Term): Set[Name] =
    term match {
      case Var(n) => Set(n)
      case Lam(n, t) => freeVars(t).filter(_ != n)
      case App(t1, t2) => freeVars(t1) ++ freeVars(t2)
    }

  def allVars(term: Term): Set[Name] = {
    term match {
      case Var(n) => Set(n)
      case Lam(_, t) => allVars(t)
      case App(t1, t2) => allVars(t1) ++ allVars(t2)
    }
  }
}
