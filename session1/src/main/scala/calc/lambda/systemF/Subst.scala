package calc.lambda.systemF

import calc.lambda.systemF.Name.Name

trait Subst[A] {
  def free: Set[Name]
  def fresh: Name
  def subst(n: Name, sub: A): A
}
