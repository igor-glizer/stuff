/*1+1
"abc" + "def"
def hello(name: String) = s"hello $name"
hello("world")


def fib(n : Int) : Int =
  if (n <= 1)
    n
  else
    fib(n-2) + fib(n-1)

fib(30)
*/



case class Person (
  firstName:String,
  lastName:String,
  age:Int)



val people = List(
  Person( "Niklaus", "Wirth",     80 ),
  Person( "Anders",  "Hejlsberg", 53 ),
  Person( "x",  "y", 4 ),
  Person( "Martin",  "Odersky",   55 ),
  Person( "Kenneth", "Thompson",  71 ) )

def printAdults(people : List[Person]) = {
  val adults = people.filter(p => p.age > 18)
  adults.foreach(p => println(p.lastName + "," + p.firstName))
}

printAdults(people)