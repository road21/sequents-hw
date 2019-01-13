package calc.lambda.untyped

import calc.lambda.untyped.TermNam.Name

object TermNam {
  type Name = (String, Int)
}

sealed trait Term
case class Var(n: Name) extends Term
case class Lam(n: Name, t: Term) extends Term
case class App(t1: Term, t2: Term) extends Term
