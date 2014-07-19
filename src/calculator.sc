//partial implementation of a calculator

import Calculator._

object Calculator
{
  trait Operand
  case class Int32(value: Int) extends Operand
  case class Real32(value: Float) extends Operand
  case class Int64(value: Long) extends Operand
  case class Real64(value: Double) extends Operand
  trait Add[T <: Operand]
  {
    def op (left : T, right : T) : T
  }
  trait Sub[T <: Operand]
  {
    def op (left : T, right : T) : T
  }
  trait Mult[T <: Operand]
  {
    def op (left : T, right : T) : T
  }
  trait Div[T <: Operand]
  {
    def op (left : T, right : T) : T
  }
  trait Rem[T <: Operand]
  {
    def op (left : T, right : T) : T
  }

  def add[T <: Operand](left : T, right : T)(implicit ev : Add[T]) =
    ev.op(left, right)

  implicit object Int32Add extends Add[Int32] {
    def op (left : Int32, right : Int32) = Int32(left.value + right.value)
  }
  implicit object Real32Add extends Add[Real32] {
    def op (left : Real32, right : Real32) = Real32(left.value + right.value)
  }

  implicit object Int64Add extends Add[Int64] {
    def op (left : Int64, right : Int64) = Int64(left.value + right.value)
  }

  implicit object Real64Add extends Add[Real64] {
    def op (left : Real64, right : Real64) = Real64(left.value + right.value)
  }
}

print(add(Int32(1), Int32(2)).value)
print(add(Real32(1), Real32(2)).value)
print(add(Int64(1), Int64(2)).value)
print(add(Real64(1), Real64(2)).value)

