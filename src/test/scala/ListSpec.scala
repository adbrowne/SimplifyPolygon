
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.ArraySeq
package net.hasnext.mapping.tests{
  import net.hasnext.mapping._
  class ListSpec extends FlatSpec with ShouldMatchers {
    "Single Part" should "return single part" in{
      val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
      val parts  = ShapeFileLoader.getParts(ArraySeq(1), ArraySeq(new Point(22,22)))
      parts.length should equal(1)
    }
  }
}
