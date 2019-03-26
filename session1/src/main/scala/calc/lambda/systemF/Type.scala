package calc.lambda.systemF

import calc.lambda.systemF.Name.Name
import cats.instances.tuple._
import cats.syntax.functor._
import cats.syntax.order._
import cats.instances.string._
import cats.instances.int._
import TypeAlg.syntax._

sealed trait Type extends Subst[Type] {
  def to[A: TypeAlg]: A
}

object Type {
  case class Var(name: Name) extends Type {
    lazy val free = Set(name)
    lazy val fresh = name.map(_ + 1)

    override def subst(n: Name, sub: Type): Type =
      if (n == name) sub else this

    override def to[A: TypeAlg]: A = TypeAlg[A].v(name)
  }

  case class Impl(t1: Type, t2: Type) extends Type {
    lazy val free = t1.free ++ t2.free
    lazy val fresh = t1.fresh max t2.fresh

    override def subst(n: Name, sub: Type): Type =
      Impl(t1.subst(n, sub), t2.subst(n, sub))

    override def to[A: TypeAlg]: A = t1.to[A] ->> t2.to[A]
  }

  case class Forall(v: Name, t: Type) extends Type {
    lazy val free = t.free - v
    lazy val fresh = t.fresh

    override def subst(n: Name, sub: Type): Type =
      if (v == n) this
      else if (sub.free.contains(v)) {
        val v1 = fresh max sub.fresh
        Forall(v1, t.subst(v, Var(v1)).subst(n, sub))
      } else Forall(v, t.subst(n, sub))

    override def to[A: TypeAlg]: A =
      TypeAlg[A].forall(v, t.to[A])
  }

  case class Product(ts: List[Type]) extends Type {
    lazy val free = ts.map(_.free).fold(Set())(_ ++ _)
    lazy val fresh = ts.map(_.fresh).fold(Name.X)(_ max _)

    override def subst(n: Name, sub: Type): Type =
      Product(ts.map(_.subst(n, sub)))

    override def to[A: TypeAlg]: A =
      TypeAlg[A].product(ts.map(_.to[A]))
  }

  sealed trait SimpleType extends Type {
    lazy val free = Set()
    lazy val fresh = Name.X

    override def subst(n: Name, sub: Type): Type = this
  }

  case object Unit extends SimpleType {
    override def to[A: TypeAlg]: A = TypeAlg[A].unit
  }

  case object Bool extends SimpleType {
    override def to[A: TypeAlg]: A = TypeAlg[A].bool
  }

  case object Int extends SimpleType {
    override def to[A: TypeAlg]: A = TypeAlg[A].int
  }
}
