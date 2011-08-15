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

      "LoadFile" should "return 8 shapes" in {
        val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
          shapeFile.shapes.length should equal (8)
      }
      "First shape" should "should be record 1" in {
        val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
          val firstShape = shapeFile.shapes.head
        firstShape.recordNumber should equal (1)
      }
      "First shape" should "should have length 624" in {
        val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
          val firstShape = shapeFile.shapes.head
        firstShape.contentLength should equal (624)
      }
      "First shape" should "should have bounding box" in {
        val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
          val firstShape = shapeFile.shapes.head
        firstShape.boundingBox should equal (
          new BoundingBox(
            new Point(148.7672119140625,-35.92083740234375), 
            new Point(149.40692138671875,-35.139442443847656)))
        }
        "First shape" should "should have 1 parts" in {
          val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
            val firstShape = shapeFile.shapes.head
          firstShape.parts.length should equal (1)
        }
        "First shape" should "should have 75 points" in {
          val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
            val firstShape = shapeFile.shapes.head
          firstShape.parts.head.points.length should equal (75)
        }
        "Shape" should "should turn into Json" in {
    import net.liftweb.json.JsonAST._
    import net.liftweb.json.Extraction._
    import net.liftweb.json.Printer._
          implicit val formats = net.liftweb.json.DefaultFormats
          val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")
            val outputString = pretty(render(decompose(shapeFile)))

            def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
            val p = new java.io.PrintWriter(f)
              try { op(p) } finally { p.close() }
          }
          val outputFile = "./map/mapdata/allscala.js"
          printToFile(new java.io.File(outputFile))(p => {
              p.println(outputString)
            })
        }
      }
    }
