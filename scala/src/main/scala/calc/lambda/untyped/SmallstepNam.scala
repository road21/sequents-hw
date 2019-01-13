package calc.lambda.untyped

import calc.lambda.untyped.TermNam.Name
import Terms._
import cats.implicits._
import cats.kernel.Order

object SmallstepNam {
  def fresh(names: List[Name]): Name = {
    names match {
      case Nil => X
      case x :: xs =>
        val (n, v) = xs.foldRight(x)(Order[Name].max)
        (n, v + 1)
    }
  }
}
