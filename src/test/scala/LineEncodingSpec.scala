import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import net.hasnext.mapping._

package net.hasnext.mapping.tests {

  class MapSpec extends FlatSpec with ShouldMatchers {
    "AddShape" should "return map with single shape" in{
      val newMap = new PolyMap
      val mapWithOneShape = newMap.addShape(new MapRegion)

        mapWithOneShape.shapes.length should equal (1)
    }

    /*"Add two shapes" should "share segments" in {
      val square1 = new MapRegion((0,0),(0,1),(1,1),(1,0))
        val square2 = new MapRegion(Lis((0,0),(0,1),(-1,1),(-1,0)))

        val map = new PolyMap(List(square1, square2))

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
      Segment((0,0),(0,1)).points.head should equal (MapPoint(0,0))
    }

    "Segment" should "be split by another segment" in {
      val segment = Segment((0,0),(0,1),(1,1),(1,0))
      val subSegment = Segment((0,1),(1,1))

      segment.splitBySegment(subSegment) should equal (List(Segment((0,0)), subSegment, Segment((1,0))))
    }
    
    "Segment" should "be split by another segment when segment is at the start" in {
      val segment = Segment((0,0),(0,1),(1,1),(1,0))
      val subSegment = Segment((0,0),(0,1))

      segment.splitBySegment(subSegment) should equal (List(subSegment, Segment((1,1),(1,0))))
    }
    
    "Segment" should "be split by another segment when segment is at the end" in {
      val segment = Segment((0,0),(0,1),(1,1),(1,0))
      val subSegment = Segment((1,1),(1,0))

      segment.splitBySegment(subSegment) should equal (List(Segment((0,0),(0,1)), subSegment))
    }

    "Segment" should "remain the same when subsegment does not match" in {
      val segment = Segment((0,0),(0,1),(1,1),(1,0))
      val subSegment = Segment((2,2),(3,3))

      segment.splitBySegment(subSegment) should equal (List(segment))
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

/*    "MapRegion" should "be reduced when combined with another" in {

      val square1 = MapRegion((0,0),(0,1),(1,1),(1,0))
      val square2 = MapRegion((0,0),(0,1),(-1,1),(-1,0))

      var square1_segmented = square1.findCommonSegments(square2)

      square1_segmented.segments should equal (2)
    } */
  }

  class LcsSpec extends FlatSpec with ShouldMatchers
  {
    "No subsequence" should "return empty seq" in {
      Utility.longestCommonSubstring(List(1,2,3), List(4,5)) should equal (List()) 
    }
    "Single element" should "return single element" in {
     Utility.longestCommonSubstring(List(1,2,3), List(3,4,5)) should equal (List(3)) 
    }
    "Two element" should "return two elements" in {
     Utility.longestCommonSubstring(List(4,5,6), List(3,4,5)) should equal (List(4,5)) 
    }
  }
  class CommonSegmentSpec extends FlatSpec with ShouldMatchers
  {
    def segmentOverlaps(segment1: Segment, segment2: Segment) = {
      val lcs = Utility.longestCommonSubstring(segment1.points, segment2.points)
      new Segment(lcs)
    }

    "CommonSegment" should "identify common segments" in {
      val square1 = Segment((0,0),(0,1),(1,1),(1,0))
      val square2 = Segment((0,0),(0,1),(-1,1),(-1,0))

      val overlap = segmentOverlaps(square1, square2)
      overlap should equal (Segment((0,0),(0,1))) 
    }
  }
}
