package calc.lambda.systemF

import calc.lambda.systemF.Name.Name
import cats.instances.tuple._
import cats.syntax.functor._
import cats.syntax.order._
import cats.instances.string._
import cats.instances.int._

sealed trait Term extends Subst[Term] {
  def isValue: Boolean
  def substT(n: Name, sub: Type): Term

  def to[A: TermAlg]: A
  def step: Option[Term]

  def reduce: List[Term] = step match {
    case Some(t) => t :: t.reduce
    case None => Nil
  }
}

object Term {
  sealed trait Value extends Term {
    val isValue = true
  }

  sealed trait Expression extends Term {
    val isValue = false
  }

  case class Var(name: Name) extends Value {
    lazy val free = Set(name)
    lazy val fresh = name.map(_ + 1)

    override def subst(n: Name, sub: Term): Term =
      if (n == name) sub else this

    override def substT(n: Name, sub: Type): Term = this
    override def to[A: TermAlg]: A = TermAlg[A].v(name)
    override def step: Option[Term] = None
  }

  case class Lam(v: Name, ty: Type, t: Term) extends Value {
    lazy val free = t.free - v
    lazy val fresh = t.fresh

    override def substT(n: Name, sub: Type): Term =
      Lam(v, ty, t.substT(n, sub))

    override def subst(n: Name, sub: Term): Term =
      if (n == v) this
      else if (sub.free.contains(v)) {
        val v1 = fresh max sub.fresh
        Lam(v1, ty, t.subst(v, Var(v1)).subst(n, sub))
      } else Lam(v, ty, t.subst(n, sub))

    override def to[A: TermAlg]: A = TermAlg[A].lam(v, ty, t.to[A])
    override def step: Option[Term] = None
  }

  case class App(t1: Term, t2: Term) extends Expression {
    lazy val free = t1.free ++ t2.free
    lazy val fresh = t1.fresh max t2.fresh

    override def subst(n: Name, sub: Term): Term =
      App(t1.subst(n, sub), t2.subst(n, sub))

    override def substT(n: Name, sub: Type): Term =
      App(t1.substT(n, sub), t2.substT(n, sub))

    override def to[A: TermAlg]: A = TermAlg[A].app(t1.to[A], t2.to[A])
    override def step: Option[Term] = t1 match {
      case Lam(x, _, t) => Some(t.subst(x, t2)) // E-AppAbs
      case _: Value => t2.step.map(App(t1, _))  // E-App2
      case _ => t1.step.map(App(_, t2))         // E-App1
    }
  }

  case class LamT(nameT: Name, term: Term) extends Value {
    lazy val free = term.free
    lazy val fresh = term.fresh

    override def subst(n: Name, sub: Term): Term = term.subst(n, sub)
    override def substT(n: Name, sub: Type): Term = ???
    override def to[A: TermAlg]: A = ???
    override def step: Option[Term] = ???
  }
}
