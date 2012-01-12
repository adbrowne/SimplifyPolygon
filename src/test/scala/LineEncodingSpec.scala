import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import net.hasnext.mapping._

package net.hasnext.mapping.tests {

  class MapSpec extends FlatSpec with ShouldMatchers {
/*    "AddShape" should "return map with single shape" in{
      val newMap = new PolyMap
      val mapWithOneShape = newMap.addShape(MapRegion((0.0,0.0)))

        mapWithOneShape.shapes.length should equal (1)
    } */
/*
    "Add two shapes" should "share PointSegments" in {
        val square1 = MapRegion((0,0),(0,1),(1,1),(1,0))
        val square2 = MapRegion((0,0),(0,1),(-1,1),(-1,0))

        val map = new PolyMap(List(square1, square2))

        
        map.segments.length should equal (3)
    } */
  }

  class PointSegmentSpec extends FlatSpec with ShouldMatchers
  {
    "PointSegment" should "have zero children" in {
      PointSegment((0,0),(0,1)).children.length should equal (0)
    }

    "PointSegment" should "be equal if points are the same" in {
      PointSegment((0,0),(0,1))  equivalentTo (PointSegment((0,0),(0,1))) should equal (true)
    }

    "PointSegment" should "return the two points for a two point PointSegment" in {
      PointSegment((0,0),(0,1)).points.length should equal (2)
    }
    
    "PointSegment" should "maintain point order" in {
      PointSegment((0,0),(0,1)).points.head should equal (MapPoint(0,0))
    }

   /* "PointSegment" should "be split by another PointSegment" in {
      val segment = PointSegment((0,0),(0,1),(1,1),(1,0))
      val subPointSegment = PointSegment((0,1),(1,1))

      segmentListsEqual(segment.splitByPointSegment(subPointSegment) , (List(PointSegment((0,0)), subPointSegment, PointSegment((1,0))))) should equal (true)
    }
  */ 
    def segmentListsEqual(left : List[PointSegment], right: List[PointSegment]) = {
      if(left.length != right.length)
      {
        false
      }
      else{
        val zipped = left zip right
        zipped forall(z => z match{
            case(x,y) => x equivalentTo y
            })
      }
    }
/*
    "PointSegment" should "be split by another PointSegment when PointSegment is at the start" in {
      val segment = PointSegment((0,0),(0,1),(1,1),(1,0))
      val subPointSegment = PointSegment((0,0),(0,1))

      segmentListsEqual(segment.splitByPointSegment(subPointSegment),    (List(subPointSegment, PointSegment((1,1),(1,0))))) should equal (true)
    }
 */   
 /*
    "PointSegment" should "be split by another PointSegment when PointSegment is at the end" in {
      val segment = PointSegment((0,0),(0,1),(1,1),(1,0))
      val subPointSegment = PointSegment((1,1),(1,0))

      segmentListsEqual(segment.splitByPointSegment(subPointSegment) , (List(PointSegment((0,0),(0,1)), subPointSegment))) should equal (true)
    }
*/
    "PointSegment" should "remain the same when subPointSegment does not match" in {
      val segment = PointSegment((0,0),(0,1),(1,1),(1,0))
      val subPointSegment = PointSegment((2,2),(3,3))

      segment.splitByPointSegment(subPointSegment) should equal (segment)
    }
  }

  class MapRegionSpec extends FlatSpec with ShouldMatchers 
  {
    /*
    "MapRegion" should "create PointSegments" in {
      val segment = PointSegment((0,0),(0,1),(1,1),(1,0))
      val shape = new MapRegion(segment)

      shape.segment should equal (segment) 
    }

    "MapRegion" should "be created by tuples" in {

      val region = MapRegion((0,0),(0,1),(1,1),(1,0))
    } */
/*
    "MapRegion" should "be reduced when combined with another" in {

      val square1 = MapRegion((0,0),(0,1),(1,1),(1,0))
      val square2 = MapRegion((0,0),(0,1),(-1,1),(-1,0))

      var square1_PointSegmented = square1.findCommonPointSegments(square2)

      square1_PointSegmented.segments.length should equal (2)
    } 
 */  
 /*
    "MapRegion" should "be stay the same when combined with another that has no overlap" in {

      val square1 = MapRegion((0,0),(0,1),(1,1),(1,0))
      val square2 = MapRegion((3,3),(3,2),(-2,2),(-2,3))

      var square1_PointSegmented = square1.findCommonPointSegments(square2)

      square1_PointSegmented.segments.length should equal (1)
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
  class CommonPointSegmentSpec extends FlatSpec with ShouldMatchers
  {
    def PointSegmentOverlaps(PointSegment1: PointSegment, PointSegment2: PointSegment) = {
      val lcs = Utility.longestCommonSubstring(PointSegment1.points, PointSegment2.points)
      new PointSegment(lcs)
    }

    "CommonPointSegment" should "identify common PointSegments" in {
      val square1 = PointSegment((0,0),(0,1),(1,1),(1,0))
      val square2 = PointSegment((0,0),(0,1),(-1,1),(-1,0))

      val overlap = PointSegmentOverlaps(square1, square2)
      overlap equivalentTo PointSegment((0,0),(0,1)) should equal (true) 
    }
    def getPointSegment(num: Int, points: Tuple2[Int,Int]*) = {
      new PointSegment(num, points map (point => point match {
          case(x: Int, y: Int) => new MapPoint(x,y)  
        }))
    }

    def addSegmentAndSimplify(simplifiedSegments : Seq[Segment], segment : Segment) : Seq[Segment] = {
      segment :: simplifiedSegments.toList
    }

    def simplifySegments(simplifiedSegments : Seq[Segment], remainingSegments : Seq[Segment]) : Seq[Segment] = remainingSegments match {
      case x::xs => 
        simplifySegments(addSegmentAndSimplify(simplifiedSegments, x), xs)
      case _ => simplifiedSegments
    }
    
    def simplifySegments(segments : Seq[Segment]) : Seq[Segment] = {
      simplifySegments(List(), segments)
    }
    
    def splitSegment(nextid: Int, segment : PointSegment, subSegment: PointSegment) : Segment = {
      segment.splitByPointSegment(nextid, subSegment) 
    }
    
    "CommonPointSegment" should "split segment" in {
      val square1 = getPointSegment(1,(0,0),(0,1),(1,1),(1,0))

      val simplified = splitSegment(3,square1,getPointSegment(2,(0,0),(0,1)))

      simplified.id should equal (1)  
      simplified.children.length should equal (2)
      var firstChild = simplified.children.head 
      firstChild equivalentTo PointSegment((0,0),(0,1)) should equal (true)
      firstChild.id should equal(2)
      var lastChild = simplified.children.last 
      lastChild equivalentTo PointSegment((1,1),(1,0)) should equal (true)
      lastChild.id should equal(3)

      val leafSegments = simplified.leafSegments
      leafSegments should contain (firstChild)
      leafSegments should contain (lastChild)
    }
    
    "CommonPointSegment" should "group if subsegment is equivalent to itself" in {
      val square1 = getPointSegment(1,(0,0),(0,1),(1,1),(1,0))
      val wholeSubSegment = getPointSegment(2,(0,0),(0,1),(1,1),(1,0))

      val simplified = splitSegment(3,square1,wholeSubSegment)

      simplified.id should equal (1)  
      simplified.children.length should equal (1)
      simplified.children.head equivalentTo square1 should equal (true)
    }
    
    "CommonPointSegment" should "return itself if subsegment is equal to itself" in {
      val square1 = getPointSegment(1,(0,0),(0,1),(1,1),(1,0))
      val wholeSubSegment = getPointSegment(2,(0,0),(0,1),(1,1),(1,0))

      val simplified = splitSegment(3,square1,wholeSubSegment)

      simplified.id should equal (1)  
      simplified.children.length should equal (1)
      simplified.children.head equivalentTo square1 should equal (true)
    }
    
    "CommonPointSegment" should "add segment and simplify" in {
      val square1 = getPointSegment(1,(0,0),(0,1),(1,1),(1,0))
      val square2 = getPointSegment(2,(0,0),(0,1),(-1,1),(-1,0))

      var polyMap = new PolyMap()
      polyMap = polyMap.addShape(square1, "Square 1")
      polyMap = polyMap.addShape(square2, "Square 2")

      polyMap.leafSegments.length should equal(3)

      polyMap.leafSegments exists (x => x equivalentTo(PointSegment((0,0),(0,1))))
    } 

/*    "CommonPointSegment" should "simplify segments" in {
      val square1 = getPointSegment(1,(0,0),(0,1),(1,1),(1,0))
      val square2 = getPointSegment(2,(0,0),(0,1),(-1,1),(-1,0))

      val simplified = simplifySegments(List(square1, square2))

      simplified.length should equal(3)

      simplified exists (x => x equivalentTo(PointSegment((0,0),(0,1))))
    } */
  }
}
