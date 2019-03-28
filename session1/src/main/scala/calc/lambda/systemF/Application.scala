package calc.lambda.systemF

import calc.lambda.systemF.Operation.{Append, Plus}
import calc.lambda.systemF.TermAlg.TermFinal
import calc.lambda.systemF.Type.{Impl, ListT, Product, Var}
import cats.Id
import cats.arrow.FunctionK
import TermAlg.instances._
import calc.lambda.systemF.Term.{IntValue, Projection, TupleN}

object Application {
  import Name._

  def main(args: Array[String]): Unit = {
    val test1: TermFinal = new FunctionK[TermAlg, Id] {
      override def apply[A](fa: TermAlg[A]): A = {
        val map = fa.lamT(A, fa.lamT(B,
          fa.lam(X, Product(ListT(Var(A)), Impl(Var(A), Var(B))),
            fa.fold(
              fa.proj(fa.v(X), 1),
              fa.list(Var(B), List()),
              fa.lam(Y, Product(Var(A), ListT(Var(B))),
                fa.binaryOp(Append,
                  fa.list(
                    Var(B),
                    List(
                      fa.app(fa.proj(fa.v(X), 2), fa.proj(fa.v(Y), 1))
                    )
                  ),
                  fa.proj(fa.v(Y), 2)
                )
              )
            )
          )
        )
        )

        val x = fa.list(Type.Int, List(fa.int(1), fa.int(2), fa.int(3), fa.int(5)))
        fa.app(
          fa.appT(fa.appT(map, Type.Int),Type.Int),
          fa.tuple(List(x, fa.lam(X, Type.Int, fa.binaryOp(Plus, fa.v(X), fa.int(1)))))
        )
      }
    }

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

    val term = test1.apply(TermAlg.instances.TermInstance)
    println(term.reduce.map(_.to[String]).mkString("\n"))
  }
}



