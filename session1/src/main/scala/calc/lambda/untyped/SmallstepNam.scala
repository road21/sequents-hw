package calc.lambda.untyped

import calc.lambda.untyped.TermNam.Name
import Terms._
import cats.implicits._
import cats.kernel.Order

object SmallstepNam {
  def fresh(names: List[Name]): Name = names match {
    case Seq() => X
    case x :: xs =>
      val (n, v) = xs.foldLeft(x)(Order[Name].max)
      (n, v + 1)
  }

  def freeVars(term: Term): Set[Name] = term match {
    case Var(n) => Set(n)
    case Lam(n, t) => freeVars(t).filter(_ != n)
    case App(t1, t2) => freeVars(t1) ++ freeVars(t2)
  }

  def allVars(term: Term): Set[Name] = term match {
    case Var(n) => Set(n)
    case Lam(_, t) => allVars(t)
    case App(t1, t2) => allVars(t1) ++ allVars(t2)
  }

  def subst(n: Name, sub: Term, term: Term): Term = term match {
    case Var(`n`) => sub
    case t: Var => t
    case l @ Lam(`n`, _) => l
    case Lam(v, t) =>
      val fvs = freeVars(t)
      if (fvs.contains(v)) {
        val v1 = fresh((fvs ++ allVars(sub)).toList)
        Lam(v, subst(n, sub, subst(v, Var(v1), t)))
      }
      else Lam(v, subst(n, sub, t))
    case App(t1, t2) => App(subst(n, sub, t1), subst(n, sub, t2))
  }

  def step(term: Term): Option[Term] = term match {
    case App(Lam(x, t), sub) =>
      Some(subst(x, sub, t))
    case App(t1: App, t2) =>
      step(t1).map(t => App(t, t2))
    case App(t1, t2) =>
      step(t2).map(t => App(t1, t))
    case _ => None
  }
}
