sealed trait DivisionResult

final case class Finite(value: Int) extends DivisionResult
final case object Infinite extends DivisionResult

object divide {
  def apply(dividend: Int, divisor: Int): DivisionResult =
    if(divisor == 0) Infinite else Finite(dividend / divisor)
}
