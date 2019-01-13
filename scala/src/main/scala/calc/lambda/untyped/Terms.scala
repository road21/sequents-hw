package calc.lambda.untyped

import calc.lambda.untyped.TermNam.Name

object Terms {
  def X: Name = ("x", 0)
  def Y: Name = ("y", 0)
  def Z: Name = ("z", 0)

  def identity: Term = Lam(X, Var(X))
}
