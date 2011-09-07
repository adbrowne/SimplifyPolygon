package net.hasnext.mapping{
  import org.scalatest.FlatSpec
  import org.scalatest.matchers.ShouldMatchers

  import com.vividsolutions.jts.geom.Coordinate;
  import com.vividsolutions.jts.geom.Geometry;
  import com.vividsolutions.jts.geom.GeometryFactory;
  import com.vividsolutions.jts.geom.LinearRing;
  import com.vividsolutions.jts.operation.union._;
  class JtsSpec extends FlatSpec with ShouldMatchers {
    "jts" should "work" in {
      def getPolygon(coordinates: Array[Coordinate]) = {
        val g2 = new GeometryFactory().createLinearRing(coordinates);
        new GeometryFactory().createPolygon(g2, Array.apply())

      }
      import scala.collection.JavaConversions._
      // create a geometry by specifying the coordinates directly
      val triangle1 = Array.apply(new Coordinate(0, 0),
        new Coordinate(10, 10), new Coordinate(0, 10), new Coordinate(0,0)) ;
      // use the default factory, which gives full double-precision
      val polygon1 = getPolygon(triangle1)
      
        val triangle2 = Array.apply(new Coordinate(0, 0),
        new Coordinate(10, 10), new Coordinate(10, 0), new Coordinate(0,0)) ;
      val polygon2 = getPolygon(triangle2)

        val untion = UnaryUnionOp.union(List(polygon1, polygon2))
        untion.toString should equal(polygon2)
    }
  }
}
