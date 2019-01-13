package calc.lambda.untyped

import calc.lambda.untyped.TermNam.Name

object TermNam {
  type Name = (String, Int)
}

trait TermAlg[C] {
  def vari(n: Name): C
  def lam(n: Name, t: C): C
  def app(t1: C, t2: C): C
}

trait Term {
  def fold[C](alg: TermAlg[C]): C
}
