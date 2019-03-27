package calc.lambda.systemF

import calc.lambda.systemF.Operation.Plus
import calc.lambda.systemF.TermAlg.TermFinal
import calc.lambda.systemF.Type.{Impl, Product, Var}
import cats.Id
import cats.arrow.FunctionK
import TermAlg.instances._

object Application {
  import Name._

  def main(args: Array[String]): Unit = {
    val x: TermFinal = new FunctionK[TermAlg, Id] {
      override def apply[A](fa: TermAlg[A]): A = {
        fa.app(
          fa.appT(
            fa.lamT(
              A,
              fa.lam(
                X,
                Product(List[Type](Impl(Var(A), Var(A)), Var(A))),
                fa.app(fa.proj(fa.v(X), 1), fa.app(fa.proj(fa.v(X), 1), fa.proj(fa.v(X), 2)))
              )
            ),
            Type.Int
          ),
          fa.tuple(List(fa.lam(X, Type.Int, fa.binaryOp(Plus, fa.v(X), fa.v(X))), fa.int(10)))
        )
      }
    }

    val term = x.apply(TermAlg.instances.TermInstance)
    println(term.to[String])
    println(term.reduce.map(_.to[String]).mkString("\n"))
  }
}



