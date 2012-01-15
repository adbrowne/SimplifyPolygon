import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

package net.hasnext.mapping.combinelines.tests {

  case class MapPoint(val x : Double, val y : Double){

    def isOn(start: MapPoint, end: MapPoint) : Boolean = {
      val epsilon = 0.0001
      this.distanceTo(start,end) < epsilon
    }
    def isOn(segment: Segment) : Boolean = {
      val lines = segment.points.zip(segment.points.tail)
        val distances = lines.map(line => line match {
          case (start, end) => this.isOn(start,end)
        })
      distances.exists(x => x)
    }

    def distanceTo(other: MapPoint) : Double = {
      val xDif = this.x - other.x
      val yDif = this.y - other.y

      math.sqrt(xDif * xDif + yDif * yDif)
    }

    // http://paulbourke.net/geometry/pointline/
    def distanceTo(lineStart: MapPoint, lineEnd: MapPoint) : Double = {
      def calculateU(x1:Double,y1:Double,x2:Double,y2:Double,x3:Double,y3:Double) = {
        val numerator = (x3 -x1) * (x2 - x1) + (y3 - y1)* (y2 - y1)
          val lineLength = lineStart distanceTo lineEnd
        val denominator = math.pow(lineLength, 2)

          numerator/denominator
      }
      val x1 = lineStart.x
      val y1 = lineStart.y
      val x2 = lineEnd.x
      val y2 = lineEnd.y
      val x3 = this.x
      val y3 = this.y

      val u = calculateU(x1,y1,x2,y2,x3,y3);

      // Line point was on the segment
      if(u >= 0 && u <= 1){
        val linePointX = x1 + u*(x2-x1)
          val linePointY = y1 + u*(y2-y1) 

          val linePoint = new MapPoint(linePointX, linePointY)

          this.distanceTo(linePoint)
      }
      // Line point was outside of the ends of the provided segment so just find the distance to the nearest end
      else{
        math.min(this.distanceTo(lineStart), this.distanceTo(lineEnd))
      }
    }

  }

  class Shape(val parts: Seq[Part])

  case class Segment(val points: Seq[MapPoint])

  case class Part(val segments: Seq[Segment])

  class PolygonMap(val shapes : Seq[Shape]){
    val segments = shapes.flatMap(s => s.parts.flatMap(p => p.segments)) 
    }

  object PolygonMap {
    def apply(shapes: Shape*) = {
      new PolygonMap(shapes)
    }
  }

  class CombineLinesSpec extends FlatSpec with ShouldMatchers {
    def NewSegment(points: Tuple2[Double,Double]*) = {
      new Segment(points.map(x => new MapPoint(x._1,x._2)))
    }

    case class PointToSegmentMapping(val index: Int, val crossIndex: Option[Int], val point: MapPoint)

    def pointOnSegment(point: MapPoint, segment: Segment) = {
      val pairs = segment.points.zip(segment.points.tail)

        val initialState : Tuple2[Option[Int], Int] = (None, 0)
        val place = pairs.foldLeft(initialState)((state, pair) => {
          val start = pair._1
          val end = pair._2
          state match {
            case (None,x) => if(point.isOn(start, end)){
              (Option(x), x+1)
            }
            else{
              (None, x + 1)
            }
            case other => other
          }})

        place._1
      }

      def getSegmentMapping(segment1: Segment, segment2: Segment) = {
        val indexMap = segment1.points.map(p => pointOnSegment(p, segment2))
        val points = indexMap.zipWithIndex.zip(segment1.points)
        points.map(p => 
          new PointToSegmentMapping(p._1._2,p._1._1,p._2)
        )
      }

      def getMatches(
        segment1Mapping: Seq[PointToSegmentMapping],
        segment2Mapping: Seq[PointToSegmentMapping]
      ) = {
            val lineUpEnd = NewSegment((0,1.5),(0,2))

            new Part(
              NewSegment((0,0),(0,1),(0,1.5)) :: lineUpEnd :: Nil
            )
      }

      def combineSegments(segment1: Segment, segment2: Segment) = {
        val segment1ToSegment2Mapping = getSegmentMapping(segment1,segment2)
        val segment2ToSegment1Mapping = getSegmentMapping(segment2,segment1)

        val segment1WithCommon = getMatches(
          segment1ToSegment2Mapping,
          segment2ToSegment1Mapping
        )
        (segment1,segment2)
      }

      "Find Common Segments" should "return the same segment when there is no relationship" in {
        val lineUp = NewSegment((0,0),(0,1))
        val lineDown = NewSegment((2,2),(2,1))
        val commonSegments = combineSegments(lineUp, lineDown)

        commonSegments._1 should equal (lineUp)
        commonSegments._2 should equal (lineDown)
      }

        "Find Common Segments" should "return the same instance where there is a complete overlap" in {
          val lineUp = NewSegment((0,0),(0,1))
            val commonSegments = combineSegments(lineUp, lineUp)

            commonSegments._1 should equal (lineUp)
          commonSegments._2 should equal (lineUp)
        }

        /*
        "Find Common Segments" should "split a segment where the second one overlaps it" in {
          val lineUp = NewSegment((0,0),(0,1),(0,2))
            val lineUpEnd = NewSegment((0,1.5),(0,2))
            val commonSegments = combineSegments(lineUp, lineUpEnd)

            commonSegments._1 should equal (
            new Part(
              NewSegment((0,0),(0,1),(0,1.5)) :: lineUpEnd :: Nil
            )
        )
      commonSegments._2 should equal (lineUpEnd)
    } */
    object SegmentStep extends Enumeration {
      val MoveNextLeft = Value
      val MoveNextRight = Value
      val AddLeftCommon = Value
      val AddRightCommon = Value
    }

    case class ParseState(
      val leftInSegment: Boolean = false,
      val rightInSegment: Boolean =  false,
      val buildingCommon: Boolean = false)

    def extractSegments(
      leftSegment: Seq[MapPoint],
      rightSegment: Seq[MapPoint],
      commands: Seq[SegmentStep.Value],
      state: ParseState,
      currentLeft: Seq[MapPoint],
      currentRight: Seq[MapPoint],
      currentCommon: Seq[MapPoint],
      leftCompleteSegments: Seq[Segment],
      rightCompleteSegments: Seq[Segment]) : (Part,Part) = {
      def recurse(
        leftSegment: Seq[MapPoint] = leftSegment,
        rightSegment: Seq[MapPoint] = rightSegment,
        commands: Seq[SegmentStep.Value] = commands.tail,
        state: ParseState = state,
        currentLeft: Seq[MapPoint] = currentLeft,
        currentRight: Seq[MapPoint] = currentRight,
        currentCommon: Seq[MapPoint] = currentCommon,
        leftCompleteSegments: Seq[Segment] = leftCompleteSegments,
        rightCompleteSegments: Seq[Segment] = rightCompleteSegments
      ) = {
        extractSegments(leftSegment, rightSegment, commands, state,
          currentLeft, currentRight, currentCommon, leftCompleteSegments, rightCompleteSegments)
      }

      def partOfNonEmptySegments(segments: Seq[Segment]) = {
        new Part(segments.filter(x=> x.points != Nil))
      }

      def getCompletedSegments(
        currentItem: MapPoint,
        currentItems: Seq[MapPoint],
        currentCompletedItems: Seq[Segment]) = {
        if(currentItems != Nil){
          currentCompletedItems :+ new Segment(currentItems :+ currentItem)
        }
        else{
          currentCompletedItems
        }
      }

      commands match {
        case Nil =>
          (
            partOfNonEmptySegments(leftCompleteSegments :+ new Segment(currentCommon) :+ new Segment(currentLeft)), 
            partOfNonEmptySegments(rightCompleteSegments :+ new Segment(currentCommon) :+ new Segment(currentRight))
          )
        case SegmentStep.MoveNextLeft :: xs  => 
            recurse(
              leftSegment = leftSegment.tail,
              currentLeft = currentLeft :+ leftSegment.head
            )
        case SegmentStep.MoveNextRight :: xs  => 
            recurse(
              rightSegment = rightSegment.tail,
              currentRight = currentRight :+ rightSegment.head
            )
        case SegmentStep.AddLeftCommon :: xs  => 
           recurse(
              leftCompleteSegments = getCompletedSegments(
                leftSegment.head,
                currentLeft,
                leftCompleteSegments
              ),
              rightCompleteSegments = getCompletedSegments(
                leftSegment.head,
                currentRight,
                rightCompleteSegments
              ),
              currentLeft = Nil,
              currentRight = Nil,
              leftSegment = leftSegment.tail,
              currentCommon = currentCommon :+ leftSegment.head
            )
        case SegmentStep.AddRightCommon :: xs  => 
            recurse(
              leftCompleteSegments = getCompletedSegments(
                rightSegment.head,
                currentLeft,
                leftCompleteSegments
              ),
              rightCompleteSegments = getCompletedSegments(
                rightSegment.head,
                currentRight,
                rightCompleteSegments
              ),
              currentLeft = Nil,
              currentRight = Nil,
              rightSegment = rightSegment.tail,
              currentCommon = currentCommon :+ rightSegment.head
            )
      }
    }

    def extractSegments(
      leftSegment: Segment, 
      rightSegment: Segment,
      commands: Seq[SegmentStep.Value]
    ) : (Part,Part) = {
      extractSegments(leftSegment.points, rightSegment.points, commands, ParseState(), Nil, Nil, Nil, Nil, Nil)
    }
    
    "Can process path list" should "combine items" in {
        val lineUp = NewSegment((0,0),(0,1))
        val lineAcross = NewSegment((0,0),(1,0))
       
        import SegmentStep._

        val result = 
          extractSegments(lineUp,lineAcross, 
            Seq(
              MoveNextLeft, 
              MoveNextLeft, 
              MoveNextRight, 
              MoveNextRight)
          )

        result._1 should equal (
          new Part(lineUp :: Nil)
        )
        result._2 should equal (
          new Part(lineAcross :: Nil)
        )
    }

    "Can process path list" should "step through lists" in {
        val lineUp = NewSegment((0,0),(0,1))
       
        import SegmentStep._

        val result = 
          extractSegments(lineUp,lineUp, 
            Seq(
              AddRightCommon, 
              AddLeftCommon, 
              AddRightCommon, 
              AddLeftCommon)
          )

        val expected = new Part(NewSegment((0,0),(0,0),(0,1),(0,1)) :: Nil)

        result._1 should equal (expected)
        result._2 should equal (expected)
    }

    "Can process path list" should "combine overlappint and non-overlapping pieces" in {
          import SegmentStep._
          val lineUp = NewSegment((0,0),(0,1),(0,2))
          val lineUpEnd = NewSegment((0,1.5),(0,2))
          val result = extractSegments(lineUp, lineUpEnd,
            Seq(
              MoveNextLeft,
              MoveNextLeft,
              AddRightCommon,
              AddRightCommon,
              AddLeftCommon))

        result._1 should equal (
            new Part(
              NewSegment((0,0),(0,1),(0,1.5)) :: 
              NewSegment((0,1.5),(0,2),(0,2)) :: 
              Nil
            )
        )
        //result._2 should equal (lineUpEnd)
    }
  }
}
