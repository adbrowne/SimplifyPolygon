import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack

class StackSpec extends FlatSpec with ShouldMatchers {
  "A Point" should "be able to calculate distance to a point below it" in{
    val point = new Point(1,2)
    val secondPoint = new Point(1,3)

    (point distanceTo secondPoint) should equal (1)
  }
  
  "A Point" should "be able to calculate to a point not just below it" in{
    val point = new Point(0,0)
    val secondPoint = new Point(3,4)

    (point distanceTo secondPoint) should equal (5)
  }
}
