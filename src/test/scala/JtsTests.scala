package net.hasnext.mapping {

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

import com.vividsolutions.jts.operation.union._;

class JtsSpec extends FlatSpec with ShouldMatchers {
  def getPolygon(coordinates: Array[Coordinate]) = {
    val g2 = new GeometryFactory().createLinearRing(coordinates);
    new GeometryFactory().createPolygon(g2, Array.apply())

  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }

  import scala.collection.JavaConversions._

  "jts" should "work" in {
    // create a geometry by specifying the coordinates directly
    val triangle1 = Array.apply(new Coordinate(0, 0),
      new Coordinate(10, 10), new Coordinate(0, 10), new Coordinate(0, 0));
    // use the default factory, which gives full double-precision
    val polygon1 = getPolygon(triangle1)

    val triangle2 = Array.apply(new Coordinate(0, 0),
      new Coordinate(10, 10), new Coordinate(10, 0), new Coordinate(0, 0));
    val polygon2 = getPolygon(triangle2)

    val untion = UnaryUnionOp.union(List(polygon1, polygon2))
    untion.toString should equal(polygon2)
  }
  "shape stuff" should "be combinable" in {
    class Polygon(val points: List[Point])
    def pointToCoordinate(point: Point) = {
      new Coordinate(point.x, point.y)
    }
    def partToCoordinates(part: Part) = {
      getPolygon((part.points map pointToCoordinate).toArray)
    }
    def ShapeToCoordinates(shape: Shape) = {
      shape.parts map partToCoordinates
    }
    def coordinateToPoint(coord: Coordinate) = {
      new Point(coord.x, coord.y)
    }
    def geometryToPolygon(geom: Geometry) = {
      geom.getCoordinates() map coordinateToPoint
    }
    def unionOfShapes2(shapes: List[Shape], geom: Geometry): Geometry = {
      shapes match {
        case List() => geom
        case y :: ys => {
          val multiPolygon = ShapeToCoordinates(y).toArray
          val headCoords = (new GeometryFactory).createMultiPolygon(multiPolygon)
          unionOfShapes2(ys, UnaryUnionOp.union(List(geom, headCoords)))
        }
      }
    }
    def unionOfShapes(shapes: List[Shape]): Geometry = {
      shapes match {
        case y :: ys => unionOfShapes2(ys, (new GeometryFactory).createMultiPolygon(ShapeToCoordinates(y).toArray))
      }
    }
    val shapeFile = ShapeFileLoader.readFile("./data/australia/australia.shp")

    //val coordinateShapes = shapeFile.shapes.take(10) flatMap ShapeToCoordinates

    val union = unionOfShapes(shapeFile.shapes) //UnaryUnionOp.union(coordinateShapes)// //

    val geoms = (0 until union.getNumGeometries) map union.getGeometryN
    val polygons = geoms map geometryToPolygon

    def simplifyPolygon(points: List[Point], epsilon: Double) = {
      Simplify.simplify(points,epsilon)
    }
    val simplifiedPolygons = polygons map(x => simplifyPolygon(x.toList, 0.05))
    import net.liftweb.json.JsonAST._
    import net.liftweb.json.Extraction._
    import net.liftweb.json.Printer._
    implicit val formats = net.liftweb.json.DefaultFormats
    val outputFile = "./map/union.js"
    printToFile(new java.io.File(outputFile))(p => {
      p.write(pretty(render(decompose(simplifiedPolygons))))
    })
    geoms.length should equal(3)

  }
}

}
