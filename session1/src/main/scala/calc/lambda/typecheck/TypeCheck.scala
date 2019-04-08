package calc.lambda.typecheck

import calc.lambda.typecheck.TypeCheck.Context
import calc.lambda.typed.Term.Name
import cats.{Bifunctor, Monoid, Functor}
import cats.instances.tuple._
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.order._
import cats.syntax.bifunctor._
import cats.syntax.functor._
import cats.syntax.monoid._

sealed trait Type
case class VarT(n: Name) extends Type {
  override def toString: String = n._1 + n._2
}
case class Impl(t1: Type, t2: Type) extends Type {
  override def toString: String = s"(${t1.toString}->${t2.toString})"
}

sealed trait Term
case class Var(n: Name) extends Term
case class Lam(n: Name, ty: Option[Type], t: Term) extends Term
case class App(t1: Term, t2: Term) extends Term

case class Subst(s: Map[Name, Type]) {
  lazy val dom: Set[Name] = s.keySet
  lazy val range: Set[Type] = s.values.toSet

  def apply(t: Type): Type = {
    t match {
      case v @ VarT(x) =>
        s.getOrElse(x, v)
      case Impl(t1, t2) =>
        Impl(apply(t1), apply(t2))
    }
  }

  def apply(c: Context): Context = c.map { case (n, t) =>
    (n, apply(t))
  }

  def apply[F[_, _]: Bifunctor](eqs: F[Type, Type]): F[Type, Type] =
    eqs.bimap(apply, apply)
}

object Subst {
  implicit val substMonoid: Monoid[Subst] = new Monoid[Subst] {
    override def empty: Subst = Subst(Map())
    override def combine(x: Subst, y: Subst): Subst =
      Subst(
        y.s ++ x.s.map { case (n, t) =>
          n -> y(t)
        }
      )
  }
}

case class ConstraintSet(eq: List[(Type, Type)], vars: Set[Name])

object TypeCheck {
  implicit def biFunForFun[F[_]: Functor, G[_, _]: Bifunctor] =
    new Bifunctor[λ[(A, B) => F[G[A, B]]]] {
      override def bimap[A, B, C, D](fab: F[G[A, B]])(f: A => C, g: B => D): F[G[C, D]] =
        fab.map(x => x.bimap(f, g))
    }

  val X = ("X", 0)
  val Y = ("Y", 0)
  val Z = ("Z", 0)

  type Context = Map[Name, Type]

  def all(s: Type): Set[Name] = {
    s match {
      case VarT(n) => Set(n)
      case Impl(m, p) => all(m) ++ all(p)
    }
  }

  def free(t: Type): Set[Name] = {
    t match {
      case VarT(n) => Set(n)
      case Impl(m, p) => free(m) ++ free(p)
    }
  }

  def fresh(s: Set[Name]): Name = {
    val (x, y) = s.fold(X)(_ max _)
    (x, y + 1)
  }

  def toFunction(m: Map[Name, Name]): Name => Name = n => m.getOrElse(n, n)

  def subst(x: Map[Name, Name], t: Type, c: ConstraintSet): (ConstraintSet, Type) = {
    val s = Subst(x.map{ case (n1, n2) => (n1, VarT(n2)) })
    val eqs = s[λ[(A, B) => List[(A, B)]]](c.eq)
    val term = s(t)
    val vars = c.vars.map(toFunction(x))

    (ConstraintSet(eqs, vars), term)
  }

  def freshN(namesToFix: Set[Name], ty1: Type, ty2: Type, old: Set[Name]): Map[Name, Name] = {
    val max = (all(ty1) ++ all(ty2) ++ old).fold(X)(_ max _)
    namesToFix.zipWithIndex.map { case (n, i) =>
      n -> (max._1, max._2 + i + 1)
    }.toMap
  }

  def typeInf2(c: Context, term: Term): Option[(ConstraintSet, Type, Term)] = {
    term match {
      case v @ Var(x) =>
        c.get(x).map((ConstraintSet(List(), Set()), _, v))
      case Lam(x, None, t) =>
        val ty1 = VarT(fresh(c.values.flatMap(free).toSet))
        typeInf2(c + (x -> ty1), t).map {
          case (cs, ty2, tn) =>
            (cs, Impl(ty1, ty2), Lam(x, Some(ty1), tn))
        }
      case l @ Lam(x, Some(ty), t) =>
        typeInf2(c, Lam(x, None, t)).map {
          case (cs, resTy, tn) =>
            (ConstraintSet((resTy, ty) :: cs.eq, cs.vars), resTy, Lam(x, Some(ty), tn))
        }

      case App(t1, t2) =>
        for {
          (cs1 @ ConstraintSet(eqs1, vs1), ty1, term1) <- typeInf2(c, t1)
          (cs2 @ ConstraintSet(eqs2, vs2), ty2, term2) <- typeInf2(c, t2)
        } yield {
          // TODO: subst type in term
          val subst1 = vs1 intersect free(ty2)
          val (cs1n @ ConstraintSet(eqs1n, vs1n), ty1n) =
            subst(freshN(subst1, ty1, ty2, vs1 ++ vs2), ty1, cs1)

          val subst2 = (vs2 intersect free(ty1n)) union (vs2 intersect vs1n)
          val (cs2n @ ConstraintSet(eqs2n, vs2n), ty2n) =
            subst(freshN(subst2, ty1, ty2, vs1n ++ vs2), ty2, cs2)

          val resT = VarT(fresh(vs1n ++ vs2n ++ c.values.flatMap(free).toSet))
          (ConstraintSet((ty1n, Impl(ty2n, resT)) :: (cs1n.eq ++ cs2n.eq),  (vs1n ++ vs2n) + resT.n), resT, App(term1, term2))
        }
    }
  }

  def typeInf(c: Context, term: Term): Option[(ConstraintSet, Type)] = {
    term match {
      case Var(x) =>
        c.get(x).map((ConstraintSet(List(), Set()), _))
      case Lam(x, None, t) =>
        val ty1 = VarT(fresh(c.values.flatMap(free).toSet))
        typeInf(c + (x -> ty1), t).map {
          case (cs, ty2) =>
            (cs, Impl(ty1, ty2))
        }
      case Lam(x, Some(ty), t) =>
        typeInf(c, Lam(x, None, t)).map {
          case (cs, resTy) =>
            (ConstraintSet((resTy, ty) :: cs.eq, cs.vars), resTy)
        }

      case App(t1, t2) =>
        for {
          (cs1 @ ConstraintSet(eqs1, vs1), ty1) <- typeInf(c, t1)
          (cs2 @ ConstraintSet(eqs2, vs2), ty2) <- typeInf(c, t2)
        } yield {
          val subst1 = vs1 intersect free(ty2)
          val (cs1n @ ConstraintSet(eqs1n, vs1n), ty1n) =
            subst(freshN(subst1, ty1, ty2, vs1 ++ vs2), ty1, cs1)

          val subst2 = (vs2 intersect free(ty1n)) union (vs2 intersect vs1n)
          val (cs2n @ ConstraintSet(eqs2n, vs2n), ty2n) =
            subst(freshN(subst2, ty1, ty2, vs1n ++ vs2), ty2, cs2)

          val resT = VarT(fresh(vs1n ++ vs2n ++ c.values.flatMap(free).toSet))
          (ConstraintSet((ty1n, Impl(ty2n, resT)) :: (cs1n.eq ++ cs2n.eq),  (vs1n ++ vs2n) + resT.n), resT)
        }
    }
  }

  def unify(eqs: List[(Type, Type)]): Option[Subst] = {
    eqs match {
      case Nil => Some(Monoid[Subst].empty)
      case (s, t) :: tail if s == t =>
        unify(tail)
      case (VarT(x), t) :: tail if !free(t).contains(x) =>
        val subst = Subst(Map(x -> t))
        unify(subst[λ[(A, B) => List[(A, B)]]](tail)).map(_ |+| subst)
      case (s, VarT(x)) :: tail if !free(s).contains(x) =>
        val subst = Subst(Map(x -> s))
        unify(subst[λ[(A, B) => List[(A, B)]]](tail)).map(_ |+| subst) // FIXME: copypaste
      case (Impl(s1, s2), Impl(t1, t2)) :: tail =>
        unify(tail ++ ((s1, t1) :: (s2, t2) :: Nil))
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
    typeInf2(Map(), Lam(X, None, Lam(Y, None, Lam(Z, None, App(App(Var(X), Var(Z)), App(Var(Y), Var(Z))) )))).map { case (cs, ty, term2) =>
      println(term2)
      println(ty)
      println(cs.vars)
      cs.eq.foreach { case (t1, t2) =>
        println(s"$t1 = $t2")
      }
      println(unify(cs.eq).map(_.apply(ty)))
      ()
    }
  }
}
