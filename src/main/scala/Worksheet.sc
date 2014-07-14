import org.scalactic._
import Accumulation._
println("h")

val l = List("1", "a", "2", "3", "z")
l.take(2)
val theBad = l.map(parse(_)).combined

theBad.map(l => println(s"... $l"))

def parse(s:String) : String Or Every[ErrorMessage] = {
  try {
    s.toInt
    Good(s)
  }catch {
    case e: Exception => Bad(One(s"$s is bad"))
  }
}
