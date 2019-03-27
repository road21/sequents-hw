package calc.lambda.systemF

import calc.lambda.systemF.Term.IntValue

sealed trait BinaryOp[X, Y, R] {
  def op(x: X, y: Y): R
}

case object Plus extends BinaryOp[IntValue, IntValue, IntValue] {
  override def op(x: IntValue, y: IntValue): IntValue = IntValue(x.v + y.v)
}

class Operation {

}
