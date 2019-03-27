package calc.lambda.systemF

import calc.lambda.systemF.Name.Name
import calc.lambda.systemF.Operation.{BinaryOp, Plus}
import cats.{Applicative, Comonad, Id}
import cats.instances.tuple._
import cats.syntax.functor._
import cats.syntax.order._
import cats.instances.string._
import cats.instances.int._
import cats.syntax.comonad._
import cats.syntax.applicative._

import scala.annotation.tailrec

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
    lazy val fresh = {
      val (x, y) = name // TODO
      (x, y + 1)
    }

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
      Lam(v, ty.subst(n, sub), t.substT(n, sub))

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

  case class LamT(nameT: Name, t: Term) extends Value {
    lazy val free = t.free
    lazy val fresh = t.fresh

    override def subst(n: Name, sub: Term): Term = t.subst(n, sub)
    override def substT(n: Name, sub: Type): Term =
      if (nameT == n) this else LamT(n, t.substT(n, sub))

    override def to[A: TermAlg]: A = TermAlg[A].lamT(nameT, t.to[A])
    override def step: Option[Term] = None
  }

  case class AppT(term: Term, `type`: Type) extends Expression {
    lazy val free = term.free
    lazy val fresh = term.fresh

    override def subst(n: Name, sub: Term): Term =
      AppT(term.subst(n, sub), `type`)

    override def substT(n: Name, sub: Type): Term =
      AppT(term, sub.subst(n, sub))

    override def to[A: TermAlg]: A =
      TermAlg[A].appT(term.to[A], `type`)

    override def step: Option[Term] = term match {
      case LamT(v, t) =>
        Some(t.substT(v, `type`))
      case _ => term.step.map(AppT(_, `type`))
    }
  }

  case class TupleN(f: List[Term]) extends Value {
    lazy val free = f.map(_.free).fold(Set())(_ ++ _)
    lazy val fresh = f.map(_.fresh).fold(Name.X)(_ max _)

    override def subst(n: Name, sub: Term): Term =
      TupleN(f.map(_.subst(n, sub)))

    override def substT(n: Name, sub: Type): Term =
      TupleN(f.map(_.substT(n, sub)))

    override def to[A: TermAlg]: A =
      TermAlg[A].tuple(f.map(_.to[A]))

    override def step: Option[Term] =
      stepList[Id](Nil, f, TupleN)
  }

  case class Projection(term: Term, n: Int) extends Expression {
    lazy val free = term.free
    lazy val fresh = term.fresh

    override def to[A: TermAlg]: A =
      TermAlg[A].proj(term.to[A], n)

    override def step: Option[Term] = term match {
      case TupleN(list) =>
        list.drop(n - 1).headOption
      case t: Expression =>
        t.step.map(Projection(_, n))
      case _ => None
    }

    override def substT(name: Name, sub: Type): Term =
      Projection(term.substT(name, sub), n)

    override def subst(name: Name, sub: Term): Term =
      Projection(term.subst(name, sub), n)
  }

  case class Record(ts: List[(String, Term)]) extends Value {
    lazy val free = ts.map(_._2.free).fold(Set())(_ ++ _)
    lazy val fresh = ts.map(_._2.fresh).fold(Name.X)(_ max _)

    override def substT(n: Name, sub: Type): Term =
      Record(ts.map { case (x, y) => (x, y.substT(n, sub)) } )

    override def to[A: TermAlg]: A =
      TermAlg[A].record(ts.map { case (x, y) => (x, y.to[A]) } )

    override def step: Option[Term] =
      stepList[(String, ?)](Nil, ts, Record)

    override def subst(n: (String, Int), sub: Term): Term =
      Record(ts.map { case (x, y) => (x, y.subst(n, sub)) } )
  }

  case class ListT(ty: Type, v: List[Term]) extends Value {
    lazy val free = v.map(_.free).fold(Set())(_ ++ _)
    lazy val fresh = v.map(_.fresh).fold(Name.X)(_ max _)
    override def subst(n: Name, sub: Term): Term = ListT(ty, v.map(_.subst(n, sub)))
    override def substT(n: Name, sub: Type): Term = ListT(ty.subst(n, sub), v.map(_.substT(n, sub)))
    override def to[A: TermAlg]: A = TermAlg[A].list(ty, v.map(_.to[A]))
    override def step: Option[Term] =
      stepList[Id](Nil, v, ListT(ty, _))
  }

  case class Fold(l: Term, e: Term, f: Term) extends Expression {
    lazy val free = l.free ++ e.free ++ f.free
    lazy val fresh = l.fresh max e.fresh max f.fresh
    override def subst(n: Name, sub: Term): Term =
      Fold(l.subst(n, sub), e.subst(n, sub), f.subst(n, sub))
    override def substT(n: Name, sub: Type): Term =
      Fold(l.substT(n, sub), e.substT(n, sub), f.substT(n, sub))
    override def to[A: TermAlg]: A = TermAlg[A].fold(l.to, e.to, f.to)

    override def step: Option[Term] = (l, e, f) match {
      case (ListT(ty, v), e: Value, l: Lam) =>
        v.foldLeft[Option[Term]](Some(e)) {
          case (Some(x), y) =>
            App(l, TupleN(List(x, y))).reduce.lastOption
          case _ => None
        }
      case _ => ???
    }
  }

  case class Field(t: Term, p: String) extends Expression {
    lazy val free = t.free
    lazy val fresh = t.fresh

    override def subst(n: Name, sub: Term): Term = t.subst(n, sub)
    override def substT(n: Name, sub: Type): Term = t.substT(n, sub)
    override def to[A: TermAlg]: A = t.to[A]
    override def step: Option[Term] = t match {
      case Record(l) =>
        l.find(_._1 == p).map(_._2)
      case _: Expression =>
        t.step.map(Field(_, p))
      case _ => None
    }
  }

  sealed trait SimpleValue extends Value {
    lazy val free = Set()
    lazy val fresh = Name.X
    override def subst(n: Name, sub: Term): Term = this
    override def substT(n: Name, sub: Type): Term = this
    override def step: Option[Term] = None
  }

  case class BooleanValue(v: Boolean) extends SimpleValue {
    override def to[A: TermAlg]: A = TermAlg[A].boolean(v)
  }

  case class IntValue(v: Int) extends SimpleValue {
    override def to[A: TermAlg]: A = TermAlg[A].int(v)
  }

  case class DoubleValue(v: Double) extends SimpleValue {
    override def to[A: TermAlg]: A = TermAlg[A].double(v)
  }

  case class StringValue(s: String) extends SimpleValue {
    override def to[A: TermAlg]: A = TermAlg[A].string(s)
  }

  abstract class BinaryOpExpr(op: BinaryOp, l: Term, r: Term) extends Expression {
    def applyOp(l: Term, r: Term): Term

    lazy val free = l.free ++ r.free
    lazy val fresh = l.fresh max r.fresh

    override def subst(n: Name, sub: Term): Term =
      applyOp(l.subst(n, sub), r.subst(n, sub))

    override def substT(n: Name, sub: Type): Term =
      applyOp(l.substT(n, sub), r.substT(n, sub))

    override def to[A: TermAlg]: A = TermAlg[A].binaryOp(op, l.to[A], r.to[A])
  }

  case class PlusOp(l: Term, r: Term) extends BinaryOpExpr(Plus, l, r) {
    def applyOp(l: Term, r: Term): Term = PlusOp(l, r)

    override def step: Option[Term] = (l, r) match {
      case (_: Expression, _) => l.step.map(PlusOp(_, r))
      case (_, _: Expression) => r.step.map(PlusOp(l, _))
      case (IntValue(x), IntValue(y)) => Some(IntValue(x + y))
      case (DoubleValue(x), DoubleValue(y)) => Some(DoubleValue(x + y))
      case _ => None
    }
  }

  @tailrec
  def stepList[F[_]: Comonad: Applicative](prefix: List[F[Term]], postfix: List[F[Term]], build: List[F[Term]] => Term): Option[Term] = postfix match {
    case h :: t =>
      val ext = h.extract
      if (ext.isValue)
        stepList(h :: prefix, t, build)
      else ext.step.map(x => build(prefix.reverse ++ (x.pure[F] :: t)))
    case Nil => None
  }
}
