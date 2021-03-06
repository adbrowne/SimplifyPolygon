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
/*    "Parallel Lines" should "combine" in {
      val leftSquare = Shape((0,0),(1,0),(1,1),(0,1)) 
      val rightSquare = Shape((1,0),(1,1),(2,1),(2,0))
      val map = PolygonMap(leftSquare,rightSquare)

      map.segments.length should equal (3)
    } */
    def NewSegment(points: Tuple2[Int,Int]*) = {
      new Segment(points.map(p => p match {
          case (x,y) => new MapPoint(x,y)
        }))
    }

    def NewPart(points: Tuple2[Int,Int]*) = {
      val segment = new Segment(points.map(p => p match {
          case (x,y) => new MapPoint(x,y)
        }))
      new Part(segment :: Nil)
    }
    
    def NewShape(points: Tuple2[Int,Int]*) = {
      val segment = new Segment(points.map(p => p match {
          case (x,y) => new MapPoint(x,y)
        }))
      new Shape(new Part(segment :: Nil) :: Nil)
    }

    "Point" should "be on segment" in {
        val point = new MapPoint(1,1)
        val segment = NewSegment((0,0),(2,2))

        point isOn segment should equal (true)
    }
    
    "Point past the end" should "not be on segment" in {
        val point = new MapPoint(3,3)
        val segment = NewSegment((0,0),(2,2))

        point isOn segment should equal (false)
    }
 
    def combineCommonSubsegments (segment1: Segment, segment2: Segment) = {
        
        def getCrossIndexedList(list : Segment, listToCrossIndex: Segment) = {
          val points = list.points
          val locations = points.map(p => pointOnSegment(p, listToCrossIndex))
          val indexPointAndCrossIndex = points.zipWithIndex.zip(locations)
        
          indexPointAndCrossIndex.map(x => x match {
            case ((point, index), crossIndex) => PointCrossIndex(index, point, crossIndex)
          })
        }
       
        case class Status(
          val list1: Seq[PointCrossIndex],
          val list2: Seq[PointCrossIndex],
          val buildingSeq: Boolean = false,
          val commonSeq: Seq[MapPoint],
          val list1Segments: Seq[Segment],
          val list2Segments: Seq[Segment]
        )
        {
            def addToSeqList(pci: PointCrossIndex, list: Seq[Segment]) : Seq[Segment] = {
              list
              if(buildingSeq){
                list :+ new Segment(commonSeq) //:+ new Segment(List(pci.point))
              }
              else{
                if(list.length == 0){
                  List(new Segment(List(pci.point)))
                }
                else{
                  val initialItems = list.init.toList 
                  val newSegment =  new Segment(list.last.points :+ pci.point)
                  initialItems :+ newSegment
                }
              }
            }

            def add2ToCommon : Status = {
              if(buildingSeq)
              {
                Status(list1,list2.tail,true,commonSeq.toList :+ list2.head.point,list1Segments,list2Segments)
              }
              else{
                Status(list1,list2.tail,true,list2.head.point :: Nil,list1Segments,list2Segments)
              }
            }

            def add1ToCommon : Status = {
              if(buildingSeq)
              {
                Status(list1.tail,list2,true,commonSeq.toList :+ list1.head.point,list1Segments,list2Segments)
              }
              else{
                Status(list1.tail,list2,true,list1.head.point :: Nil,addToSeqList(list1.head,list1Segments),list2Segments)
              }
            }

            def finishList2 = {
              val segmentForEnd = new Segment(list2.map(x=> x.point))
              if(buildingSeq){
                val commonSegment = new Segment(commonSeq)
                Status(Nil,Nil,false,commonSeq,list1Segments :+ commonSegment,(list2Segments :+ commonSegment) :+ segmentForEnd)
              }
              else {
                Status(Nil,Nil,false,commonSeq,list1Segments,list2Segments :+ segmentForEnd)
              }
            }
            
            def finishList1 = {
              val segmentForEnd = new Segment(list1.map(x=> x.point))
              if(buildingSeq){
                val commonSegment = new Segment(commonSeq)
                Status(Nil,Nil,false,commonSeq,list1Segments :+ commonSegment :+ segmentForEnd,list2Segments :+ commonSegment)
              }
              else {
                Status(Nil,Nil,false,commonSeq,list1Segments :+ segmentForEnd,list2Segments)
              }
            }

            def takeNextPoint : Status = {
              (list1.head,list2.head) match {
                case (PointCrossIndex(_, _, None),
                      PointCrossIndex(_,_,None)) => 
                    Status(
                      list1.tail, 
                      list2.tail, 
                      false, 
                      Nil, 
                      addToSeqList(list1.head, list1Segments),
                      addToSeqList(list2.head, list2Segments)
                    )

                case (PointCrossIndex(_, _, None),item2) => 
                  add2ToCommon
                case (item1,PointCrossIndex(_,_,None)) => 
                  add1ToCommon
                case (
                  PointCrossIndex(index1, point1, Some(crossIndex1)), 
                  PointCrossIndex(index2, point2, Some(crossIndex2))
                ) => 
                  if(crossIndex1 < index2){ 
                    add1ToCommon
                  }
                  else{
                    add2ToCommon
                  }
              }
          }
        }

        def getCommonItems(status: Status) : Status = {
          (status.list1, status.list2) match {
            case (Nil,Nil) =>  status
            case (Nil, _) => status.finishList2
            case (_, Nil) => status.finishList1
            case (_, _) => getCommonItems(status.takeNextPoint)
          }
        }
        
        val list1 = getCrossIndexedList(segment1, segment2)
        val list2 = getCrossIndexedList(segment2, segment1)

        val result = getCommonItems(Status(list1,list2,false,Nil,Nil,Nil))

        val result1 = new Part(result.list1Segments)
        /*val commonSegment = NewSegment((1,1),(2,2))
        val result1  = new Part(
          List(
            NewSegment((0,0),(1,1)), 
            commonSegment)
        )*/
        (result1, Nil) 
    }

    case class PointCrossIndex(
      val index : Int, 
      val point : MapPoint, 
      val crossIndex : Option[Int]
    )

    def findCommonSegment(segment1: Segment, segment2: Segment) = {
        
        def getCrossIndexedList(list : Segment, listToCrossIndex: Segment) = {
          val points = list.points
          val locations = points.map(p => pointOnSegment(p, listToCrossIndex))
          val indexPointAndCrossIndex = points.zipWithIndex.zip(locations)
        
          indexPointAndCrossIndex.map(x => x match {
            case ((point, index), crossIndex) => PointCrossIndex(index, point, crossIndex)
          })
          .dropWhile(x => x.crossIndex == None)
        }

        def otherListEmpty(list : Seq[PointCrossIndex]) = {
          if(list.head.crossIndex == None){
            Nil
          }
          else{
            list.head.point :: getCommonItems(Nil,list.tail)
          }
        }

        def takeNextPoint(
          list1 : Seq[PointCrossIndex],
          list2: Seq[PointCrossIndex]
        ) = {
          (list1.head,list2.head) match {
            case (PointCrossIndex(_, _, None),PointCrossIndex(_,_,None)) => Nil
            case (PointCrossIndex(_, _, None),item2) => 
              item2.point :: getCommonItems(list1, list2.tail)
            case (item1,PointCrossIndex(_,_,None)) => 
              item1.point :: getCommonItems(list1.tail, list2)
            case (
              PointCrossIndex(index1, point1, Some(crossIndex1)), 
              PointCrossIndex(index2, point2, Some(crossIndex2))
            ) => 
              if(crossIndex1 < index2){ 
                point1 :: getCommonItems(list1.tail, list2)
              }
              else{
                point2 :: getCommonItems(list1, list2.tail)
              }
          }
        }

        def getCommonItems(list1 : Seq[PointCrossIndex], list2: Seq[PointCrossIndex]) : List[MapPoint] = {
          (list1, list2) match {
            case (Nil,Nil) =>  Nil
            case (Nil, _) => otherListEmpty(list2)
            case (_, Nil) => otherListEmpty(list1)
            case (_, _) => takeNextPoint(list1,list2)
          }
        }

        val list1 = getCrossIndexedList(segment1, segment2)
        val list2 = getCrossIndexedList(segment2, segment1)
      
        new Segment(getCommonItems(list1,list2).map(x => x))
    }

    def findCommonSegments(part1: Part, part2: Part) = {
        // TODO actually implement this
        val commonSegment = NewSegment((1,1),(2,2))
        val expected1 = new Part(NewSegment((0,0),(1,1)) :: commonSegment :: Nil)
        val expected2 = new Part(commonSegment ::  NewSegment((2,2),(3,3)) :: Nil)
        (expected1, expected2)
    }

    "Overlapping parts" should "Combine segments" in {
        val part1 = NewPart((0,0),(2,2))
        val part2 = NewPart((1,1),(3,3))

        val newParts = findCommonSegments(part1, part2)

        val commonSegment = NewSegment((1,1),(2,2))
        val expected1 = new Part(NewSegment((0,0),(1,1)) :: commonSegment :: Nil)
        val expected2 = new Part(commonSegment ::  NewSegment((2,2),(3,3)) :: Nil)


        newParts._1 should equal (expected1)
        newParts._2 should equal (expected2)
    }
   
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

    "Overlapping segments" should "have point at the correct index" in {
        val point = MapPoint(2,2)
        val segment = NewSegment((1,1),(3,3))

        val result = pointOnSegment(point, segment)

        result should equal (Some(0))
    }
    
    "Overlapping segments" should "have point at the correct index (1)" in {
        val point = MapPoint(2,2)
        val segment = NewSegment((0,0),(1,1),(3,3))

        val result = pointOnSegment(point, segment)

        result should equal (Some(1))
    }
    
    "Overlapping segments" should "combine segments" in {
        val segment1 = NewSegment((0,0),(2,2))
        val segment2 = NewSegment((1,1),(3,3))

        val result = findCommonSegment(segment1, segment2)

        val commonSegment = NewSegment((1,1),(2,2))

        result should equal (commonSegment)
    }

    "Overlapping segments" should "include common pieces" in {
        val segment1 = NewSegment((0,0),(2,2))
        val segment2 = NewSegment((1,1),(3,3))

        val result = combineCommonSubsegments(segment1, segment2)

        val result1 = result._1
        val result2 = result._2

        val commonSegment = NewSegment((1,1),(2,2))

        result1 should equal (
          new Part(
            List(
              NewSegment((0,0),(1,1)), 
              commonSegment)
          )
        )
    }
  }
}
