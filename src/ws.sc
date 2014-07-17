//implicit ex, Sorter

trait Comperator[T]
{
  def isSmaller (left : T, right : T) : Boolean
}



object Sorter {
  def sort[T](arr: Array[T]) ( implicit ev : Comperator[T]) = {
    for (i <- 0 until arr.length)
      for (j <- i + 1 until arr.length)
        if (ev.isSmaller(arr(j),arr(i)))
        {
          val temp = arr(j)
          arr(j) = arr(i)
          arr(i) = temp
        }
    arr
  }
}

implicit object IntComperator extends Comperator[Int] {
  def isSmaller (left : Int, right : Int) = left < right
}

val arr = Array(1,6,7,23,5)

val grr = Sorter.sort(arr)
println(grr)