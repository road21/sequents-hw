package calc.lambda.untyped

import calc.lambda.untyped.TermNam.Name

object Terms {
  def X: Name = ("x", 0)

  def identity: Term = new Term {
    override def fold[C](alg: TermAlg[C]): C =
      alg.lam(X, alg.vari(X))
  }
}
