import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack

class StackSpec extends FlatSpec with ShouldMatchers {
  "A Point" should "be able to calculate distance to a point below it" in{
    val point = new Point(1,2)
      val secondPoint = new Point(1,3)

      (point distanceTo secondPoint) should equal (1)
  }

  "Two points" should "be equal when the numbers are the same" in{
    (new Point(1,1)) should equal(new Point(1,1))
  }

  "A Point" should "be able to calculate to a point not just below it" in{
    val point = new Point(0,0)
      val secondPoint = new Point(3,4)

      (point distanceTo secondPoint) should equal (5)
  }

  "A Point" should "be able to calculate this distance to a line" in {
    val point = new Point(0,0)
      val startPoint = new Point(1,-1)
      val endPoint = new Point(1,1)

      point.distanceTo(startPoint,endPoint) should equal(1)
  }

  "A Point" should "be zero distance to a line it's on" in {
    val point = new Point(1,0)
      val startPoint = new Point(1,-1)
      val endPoint = new Point(1,1)

      point.distanceTo(startPoint,endPoint) should equal(0)
  }

  "A Point's distance" should "be to the nearest end not an infinite line" in{
    val point = new Point(2,0)
      val startPoint = new Point(0,0)
      val endPoint = new Point(1,0)

      point.distanceTo(startPoint,endPoint) should equal(1)
  }

  "A point's distance" should "be equal to the distance to the point where startPoint and endPoint are the same" in{
    val point = new Point(2,0)
      val startAndEndPoint = new Point(0,0)

      point.distanceTo(startAndEndPoint, startAndEndPoint) should equal(2)
  }

  "A single point line" should "return itself" in {
    val point = new Point(0,0)
      val line = List(point)
      (Simplify.simplify(line,1)) should equal(line)
  }

  "A two point line" should "return itself" in {
    val startPoint = new Point(0,0)
      val endPoint = new Point(2,0)
      val line = List(startPoint, endPoint)
      (Simplify.simplify(line,1)) should equal(line)
  }

  "A three point line" should "remove middle point when it's on the line" in {
    val startPoint = new Point(0,0)
      val midPoint = new Point(1,0)
      val endPoint = new Point(2,0)
      val line = List(startPoint, midPoint, endPoint)
      (Simplify.simplify(line,1)) should equal(List(startPoint,endPoint))
  }

  "A three point line" should "leave the middle point when it's closer than epsilon from the line" in {
    val startPoint = new Point(0,0)
      val midPoint = new Point(1,0.9)
      val endPoint = new Point(2,0)
      val line = List(startPoint, midPoint, endPoint)
      (Simplify.simplify(line,1)) should equal(List(startPoint,endPoint))
  }

  "A three point line" should "leave the middle point when it's further than epsilon from the line" in {
    val startPoint = new Point(0,0)
      val midPoint = new Point(1,1.1)
      val endPoint = new Point(2,0)
      val line = List(startPoint, midPoint, endPoint)
      (Simplify.simplify(line,1)) should equal(line)
  }

  "A four point line" should "remove the middle points where they are both near the line" in {
    val startPoint = new Point(0,0)
      val midPoint1 = new Point(1,0.1)
      val midPoint2 = new Point(2,0.1)
      val endPoint = new Point(3,0)
      val line = List(startPoint, midPoint1,midPoint2, endPoint)
      (Simplify.simplify(line,1)) should equal(List(startPoint, endPoint))
  }

  "A five point line" should " recusively simplify to leave some points" in{
    val startPoint = new Point(0,0);
    val endPoint = new Point(8,0);
    val midPoint = new Point(4,3);
    val line = List(
      startPoint,
      new Point(2.4,1.2),
      midPoint,
      new Point(6.4,2),
      endPoint
    )
  (Simplify.simplify(line,1)) should equal(List(startPoint, midPoint,endPoint))
}
}
