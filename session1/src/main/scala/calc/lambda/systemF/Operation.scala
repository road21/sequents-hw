package calc.lambda.systemF

object Operation {
  sealed trait BinaryOp
  case object Plus extends BinaryOp
//  case object IfThenElse extends BinaryOp

  sealed trait UnaryOp
  case object ToDouble extends UnaryOp
}
