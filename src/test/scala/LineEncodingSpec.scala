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

  class LcsSpec extends FlatSpec with ShouldMatchers
  {
    def longestCommonSubstring[A](list1: Seq[A], list2: Seq[A]) : Seq[A] = {
      // adapted from the haskell implementation here: 
      // http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring
      
      def g[A](z: Seq[A], xy: Tuple2[A,A]) = {
        xy match {
          case (x,y) => if(x == y){
                          z ++ List(x)
                        }
                        else{
                          List()
                        }
        }
      }
      def f[A](xs: Seq[A], ys: Seq[A]) = {
        xs.zip(ys).scanLeft(Seq[A]())(g)
      }
      val substrings = (for(xs <- list1.tails; ys <- list2.tails.drop(1))
            yield(f(xs,list2) ++ f(list1,ys))).flatten

      substrings.foldLeft(Seq[A]())(
        (currentLongest, x) => {
          if(x.length > currentLongest.length){
            x
          }
          else{
            currentLongest
          }
        })
    }
    "Single element" should "return single element" in {
      longestCommonSubstring(List(1,2,3), List(3,4,5)) should equal (List(3)) 
    }
    "Two element" should "return two elements" in {
      longestCommonSubstring(List(4,5,6), List(3,4,5)) should equal (List(4,5)) 
    }
  }
  class CommonSegmentSpec extends FlatSpec with ShouldMatchers
  {
    def segmentOverlaps(segment1: Segment, segment2: Segment) = {
      Segment((0,0), (0,1))
    }

    "CommonSegment" should "identify common segments" in {
      val square1 = Segment((0,0),(0,1),(1,1),(1,0))
      val square2 = Segment((0,0),(0,1),(-1,1),(-1,0))

      val overlap = segmentOverlaps(square1, square2)
      overlap should equal (Segment((0,0),(0,1))) 
    }
  }
}
