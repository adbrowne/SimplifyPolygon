import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

package net.hasnext.mapping.tests {
  case class Point(val x: Float, val y: Float)

  object Point {
    def apply(point: Tuple2[Int,Int]) = {
      point match {
        case (x,y) => new Point(x,y)
      }
    } 

  }
  class Segment(val points : Seq[Point]){
    def this() = this(List())
    override def equals(other: Any) = other match {
      case that: Segment =>
        this.points == that.points
      case _ =>
        false
    }
  }

  object Segment {
    def apply(points: Tuple2[Int,Int]*) = {
      new Segment(points map (x=> Point(x))) 
    }
  }
  class MapRegion(val segments : List[Segment]) {
    def this() = this(List())
    }

  object MapRegion{
    def apply(points: Tuple2[Int,Int]*) = {
      // TODO: Fix the duplication here. Should be able to call the segment singleton constructor
      new MapRegion(List(new Segment(points map (x => Point(x)))))
    }
  }
  class Map(val shapes: List[MapRegion]){
    def this() = this(List())
      def addShape(shape: MapRegion) : Map = {
      new Map(shape::shapes)
    }
  }

  class MapSpec extends FlatSpec with ShouldMatchers {
    "AddShape" should "return map with single shape" in{
      val newMap = new Map
      val mapWithOneShape = newMap.addShape(new MapRegion)

        mapWithOneShape.shapes.length should equal (1)
    }

    /*"Add two shapes" should "share segments" in {
      val square1 = new MapRegion((0,0),(0,1),(1,1),(1,0))
        val square2 = new MapRegion(Lis((0,0),(0,1),(-1,1),(-1,0)))

        val map = new Map(List(square1, square2))

        map.segments.length should equal (3)
    }*/
  }

  class SegmentSpec extends FlatSpec with ShouldMatchers
  {
    "Segment" should "be equal if points are the same" in {
      Segment((0,0),(0,1)) should equal (Segment((0,0),(0,1)))
    }

    "Segment" should "return the two points for a two point segment" in {
      Segment((0,0),(0,1)).points.length should equal (2)
    }
    
    "Segment" should "maintain point order" in {
      Segment((0,0),(0,1)).points.head should equal (Point(0,0))
    }
  }

  class MapRegionSpec extends FlatSpec with ShouldMatchers 
  {
    "MapRegion" should "create segments" in {
      val segment = Segment((0,0),(0,1),(1,1),(1,0))
        val shape = new MapRegion(List(segment))

        shape.segments.head should equal (segment) 
    }

    "MapRegion" should "be created by tuples" in {

      val region = MapRegion((0,0),(0,1),(1,1),(1,0))
    }
  }

  class CommonSegmentSpec extends FlatSpec with ShouldMatchers
  {
    "CommonSegment" should "identify common segments" in {
      
    }
  }
}
