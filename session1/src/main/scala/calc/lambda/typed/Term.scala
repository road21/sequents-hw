package calc.lambda.typed

import calc.lambda.typed.Term.Name
import cats.kernel.Order
import cats.implicits._

sealed trait Type {
  def ~>(ty: Type): Type = Imp(this, ty)
  def ×(ty: Type): Type = Product(this, ty)
}

case class Imp(t1: Type, t2: Type) extends Type
case class Product(t1: Type, t2: Type) extends Type
case object Unit extends Type
case object IntTy extends Type
case class VarT(x: Name) extends Type
case class Forall(x: Name, t: Type) extends Type

object Term {
  type Name = (String, Int)
}

sealed trait Term
case class Var(x: Name) extends Term
case class Lam(x: Name, T: Type, t: Term) extends Term
case class App(t1: Term, t2: Term) extends Term
case class Tuple(t1: Term, t2: Term) extends Term
case class Proj(p: Projection, t: Term) extends Term
case object UnitT extends Term
case class IntOp(op: IntOperation, t1: Term, t2: Term) extends Term
case class IntT(value: Int) extends Term
case class AppT(t: Term, ty: Type) extends Term
case class LamT(x: Name, t: Term) extends Term

sealed trait Projection
case object Fst extends Projection
case object Snd extends Projection

sealed trait IntOperation
case object Plus extends IntOperation
case object Minus extends IntOperation

object Terms {
  def X: Name = ("x", 0)
  def Y: Name = ("y", 0)
  def Z: Name = ("z", 0)
  def A: Name = ("A", 0)
  def B: Name = ("B", 0)
  def C: Name = ("C", 0)
  def identity: Term = LamT(A, Lam(X, VarT(A), Var(X)))
}

object SmallstepNam {
  def isVal(term: Term): Boolean = term match {
    case _: App | _: Proj | _: IntOp | _: AppT => false
    case _ => true
  }

  def fresh(names: List[Name]): Name = names match {
    case Seq() => Terms.X
    case x :: xs =>
      val (n, v) = xs.foldLeft(x)(Order[Name].max)
      (n, v + 1)
  }

  def freeVars(term: Term): Set[Name] = term match {
    case Var(n) => Set(n)
    case Lam(n, _, t) => freeVars(t).filter(_ != n)
    case App(t1, t2) => freeVars(t1) ++ freeVars(t2)
    case Tuple(t1, t2) => freeVars(t1) ++ freeVars(t2)
    case Proj(_, t) => freeVars(t)
    case AppT(t, _) => freeVars(t)
    case LamT(_, t) => freeVars(t)
    case _ => Set()
  }

  def freeVarsT(t: Type): Set[Name] = t match { // same stuff for Types
    case VarT(n) => Set(n)
    case Forall(n, ty) => freeVarsT(ty).filter(_ != n)
    case Imp(t1, t2) => freeVarsT(t1) ++ freeVarsT(t2)
    case Product(t1, t2) => freeVarsT(t1) ++ freeVarsT(t2)
    case _ => Set()
  }

  def allVars(term: Term): Set[Name] = term match {
    case Var(n) => Set(n)
    case Lam(_, _, t) => allVars(t)
    case App(t1, t2) => allVars(t1) ++ allVars(t2)
    case Tuple(t1, t2) => allVars(t1) ++ allVars(t2)
    case Proj(_, t) => allVars(t)
    case AppT(t, _) => allVars(t)
    case LamT(_, t) => allVars(t)
    case _ => Set()
  }

  def allVarsT(t: Type): Set[Name] = t match {
    case VarT(n) => Set(n)
    case Forall(n, ty) => freeVarsT(ty)
    case Imp(t1, t2) => freeVarsT(t1) ++ freeVarsT(t2)
    case Product(t1, t2) => freeVarsT(t1) ++ freeVarsT(t2)
    case _ => Set()
  }

  def subst(n: Name, sub: Term, term: Term): Term = term match {
    case Var(`n`) => sub
    case l @ Lam(`n`, _, _) => l
    case Lam(v, ty, t) =>
      val fvs = freeVars(sub)
      if (fvs.contains(v)) {
        val v1 = fresh((fvs ++ allVars(term)).toList)
        Lam(v1, ty, subst(n, sub, subst(v, Var(v1), t)))
      }
      else Lam(v, ty, subst(n, sub, t))
    case App(t1, t2) => App(subst(n, sub, t1), subst(n, sub, t2))
    case Tuple(t1, t2) => Tuple(subst(n, sub, t1), subst(n, sub, t2))
    case Proj(p, t) => Proj(p, subst(n, sub, t))
    case IntOp(op, t1, t2) => IntOp(op, subst(n, sub, t1), subst(n, sub, t2))
    case AppT(t, ty) => AppT(subst(n, sub, t), ty)
    case LamT(x, t) => LamT(x, subst(n, sub, t))
    case t => t
  }

  def substT(n: Name, sub: Type, ty: Type): Type = ty match {
    case VarT(`n`) => sub
    case l @ Forall(`n`, _) => l
    case Forall(v, t) =>
      val fvs = freeVarsT(sub)
      if (fvs.contains(v)) {
        val v1 = fresh((fvs ++ allVarsT(ty)).toList)
        Forall(v1, substT(n, sub, substT(v, VarT(v1), t)))
      } else Forall(v, substT(n, sub, t))
    case Imp(t1, t2) =>
      Imp(substT(n, sub, t1), substT(n, sub, t2))
    case Product(t1, t2) =>
      Product(substT(n, sub, t1), substT(n, sub, t2))
    case t => t
  }

  def substTInTerm(n: Name, sub: Type, root: Term): Term = root match {
    case Lam(x, ty, term) =>
      Lam(x, substT(n, sub, ty), substTInTerm(n, sub, term))
    case AppT(term, ty) =>
      AppT(term, substT(n, sub, ty))
    case l @ LamT(`n`, _) => l
    case LamT(x, t) => LamT(x, substTInTerm(n, sub, t))
    case t => t
  }


  def step(term: Term): Option[Term] = term match {
    case App(Lam(x, _, t), sub) =>
      Some(subst(x, sub, t))
    case App(t1, t2) =>
      if (isVal(t1))
        step(t2).map(t => App(t1, t))
      else step(t1).map(t => App(t, t2))

    case Tuple(t1, t2) =>
      if (isVal(t1))
        step(t2).map(t => Tuple(t1, t))
      else step(t1).map(t => Tuple(t, t2))
    case Proj(Fst, Tuple(t1, _)) => Some(t1)
    case Proj(Snd, Tuple(_, t2)) => Some(t2)
    case Proj(p, t) if !isVal(t) => step(t).map(t => Proj(p, t))

    case IntOp(Plus, IntT(x), IntT(y)) => Some(IntT(x + y))
    case IntOp(Minus, IntT(x), IntT(y)) => Some(IntT(x - y))
    case IntOp(op, t1, t2) =>
      if (isVal(t1)) step(t2).map(t => IntOp(op, t1, t))
      else step(t1).map(t => IntOp(op, t, t2))

    case AppT(LamT(v, t), ty) =>
      Some(substTInTerm(v, ty, t))
    case AppT(t, ty) =>
      step(t).map(t => AppT(t, ty))

    case _ => None
  }

  def stepN(term: Term): List[Term] = step(term) match {
    case Some(t) => t :: stepN(t)
    case None => Nil
  }

  def printName(v: Name): String =
    v._1 + (if (v._2 == 0) "" else s"${v._2}")

  def printType(ty: Type): String = {
    ty match {
      case Imp(t1, t2) => s"(${printType(t1)}->${printType(t2)})"
      case Product(t1, t2) => s"(${printType(t1)} × ${printType(t2)})"
      case Unit => "Unit"
      case IntTy => "Int"
      case VarT(x) => printName(x)
      case Forall(x, t) => s"(∀${printName(x)}. ${printType(t)})"
    }
  }

  def printTerm(term: Term): String = {
    term match {
      case Var(v) => printName(v)
      case Lam(x, ty, t) => s"(${printName(x)}:${printType(ty)}) => ${printTerm(t)}"
      case App(t1, t2) => s"(${printTerm(t1)})(${printTerm(t2)})"
      case Tuple(t1, t2) => s"(${printTerm(t1)}, ${printTerm(t2)})"
      case Proj(Fst, t) => s"${printTerm(t)}._1"
      case Proj(Snd, t) => s"${printTerm(t)}._2"
      case UnitT => "unit"
      case IntOp(Plus, t1, t2) => s"(${printTerm(t1)} + ${printTerm(t2)})"
      case IntOp(Minus, t1, t2) => s"(${printTerm(t1)} - ${printTerm(t2)})"
      case IntT(x) => x.toString
      case LamT(v, t) => s"forall ${printName(v)}. ${printTerm(t)}"
      case AppT(ter, ty) => s"${printTerm(ter)} [${printType(ty)}]"
    }
  }
}

object App {
  import Terms._
  import SmallstepNam._

  def main(args: Array[String]): Unit = {
    val app3 = LamT(A, Lam(X, (VarT(A) ~> VarT(A)) × VarT(A), App(Proj(Fst, Var(X)), App(Proj(Fst, Var(X)), App(Proj(Fst, Var(X)), Proj(Snd, Var(X)))))))
    println("Apply 3 times function:" + printTerm(app3))
    val multiply8 = Lam(Z, IntTy, App(AppT(app3, IntTy), Tuple(Lam(Y, IntTy, IntOp(Plus, Var(Y), Var(Y))), Var(Z))))
    println("Multiply 8 function: " + printTerm(multiply8))

    println("3 * 8 term: ")
    println(SmallstepNam.stepN(App(multiply8, IntT(3))).map(printTerm).mkString("\n"))
  }
}
