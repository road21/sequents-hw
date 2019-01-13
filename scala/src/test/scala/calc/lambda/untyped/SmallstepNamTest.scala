package calc.lambda.untyped

import org.scalatest.{FlatSpec, Matchers}
import Terms._
import SmallstepNam._

class SmallstepNamTest extends FlatSpec with Matchers {
  val term = new Term {
    override def fold[C](alg: TermAlg[C]): C =
      alg.lam(X, alg.lam(Y, alg.app(alg.vari(Y), alg.vari(Z))))
  }

  "fresh" should "returns only fresh variables" in {
    val terms = List(("x", 0), ("y", 0), ("x", 2), ("z", 4))
    val f = fresh(terms)
    terms.find(_ == f) should be (None)
  }

  "freeVars" should "returns only free variables" in {
    freeVars(Terms.identity) should be (Set())
    freeVars(term) should be (Set(Z))
  }

  "allVars" should "return all variables" in {
    allVars(Terms.identity) should be (Set(X))
    allVars(term) should be (Set(Y, Z))
  }
}
