import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
package net.hasnext.mapping.tests{
  import net.hasnext.mapping._
  class ReadShapeFileSpec extends FlatSpec with ShouldMatchers {
    "LoadFile" should "to load file" in{
      val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
      shapeFile.fileLength should equal (80386)
    }
    "LoadFile" should "fill bounding box" in{
      val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
      shapeFile.boundingBox should equal (
        new BoundingBox(
          new Point(112.90721130371094,-54.75389099121094), 
          new Point(159.10189819335938,-10.051389694213867)))
    }

    "LoadFile" should "return polygon" in {
      
      val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
        shapeFile.shapeType should equal(5)
    }
  }
}
