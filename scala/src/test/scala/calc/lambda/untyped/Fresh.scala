package calc.lambda.untyped

import org.scalatest.{FlatSpec, Matchers}

class Fresh extends FlatSpec with Matchers {
  "fresh" should "returns only fresh variables" in {
    val terms = List(("x", 0), ("y", 0), ("x", 2), ("z", 4))
    val fresh = SmallstepNam.fresh(terms)
    terms.find(_ == fresh) should be (None)
  }
}
