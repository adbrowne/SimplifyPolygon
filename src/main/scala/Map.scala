package net.hasnext.mapping{
  object Utility {
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

  }

  case class MapPoint(val x: Double, val y: Double)
  {
    override def toString = "(" + x + "," + y + ")"
    override def hashCode = 41 * (41 + x.toInt) + y.toInt

    val tolerance = 0.0005

    def withinTolerance(left: Double,right: Double) = {
      math.abs(left - right) < tolerance
    }
    override def equals(other: Any) = other match {
      case that: MapPoint =>
      (that canEqual this) &&
      (withinTolerance(this.x, that.x)) && (withinTolerance(this.y,that.y))
      case _ =>
      false
    }
    def canEqual(other: Any) = other.isInstanceOf[MapPoint]
  }

  object MapPoint {
    def apply(point: Tuple2[Double,Double]) = {
      point match {
        case (x,y) => new MapPoint(x,y)
      }
    } 

  }

  class PointSegment(val id : Int = 0, val points : Seq[MapPoint]) extends Segment {
    def assignId(newId : Int) = {
      new PointSegment(newId, this.points)
    }
    def this() = this(0, List());
    def this(points: Seq[MapPoint]) = this(0,points)
      val children = List()
      def splitByPointSegment(subPointSegment: PointSegment) : Segment = splitByPointSegment(0, subPointSegment)

      def leafSegments = {
      List(this)
    }
    def splitByPointSegment(nextid: Int, subPointSegment: PointSegment) : Segment = {
      val index = points.indexOfSlice(subPointSegment.points)

        var varNextId = nextid;
      if(index == -1 || this == subPointSegment) {
        this
      }
      else{
        val initialSequenceLength = index
        val initial = points.take(index)
          val subPointSegmentLength = subPointSegment.points.length
        val leftOver = new PointSegment(varNextId, points.drop(initialSequenceLength + subPointSegmentLength))
          varNextId = varNextId + 1

        val rest = leftOver.points.length match {
          case 0 => Nil
          case _ => leftOver.splitByPointSegment(varNextId, subPointSegment)
        }
        if(index > 0){
          val pointId = varNextId;
          varNextId = varNextId + 1
          var initialSegment = new PointSegment(pointId, initial) 
            val children = leftOver.points.length match {
            case 0 => List(initialSegment,subPointSegment)
            case _ => List(initialSegment,subPointSegment,leftOver.splitByPointSegment(varNextId, subPointSegment))
          }
          new GroupSegment(this.id, children)
        }
        else{
          val children = leftOver.points.length match {
            case 0 => List(subPointSegment)
            case _ => List(subPointSegment,leftOver.splitByPointSegment(varNextId, subPointSegment))
          }
          new GroupSegment(this.id,children)
        }
      }
    }
    def equivalentTo(other: Segment) = other match {
      case other: PointSegment =>
      this.points == other.points
      case _ => 
      false
    }
    override def equals(other: Any) = other match {
      case that: PointSegment =>
      this.id == that.id && this.id != 0 && that.id != 0      
      case _ => false
    }

    override def toString = {
      "PointSegment(" + points.toString + ")"
    }
  }

  object PointSegment {
    def apply(points: Tuple2[Double,Double]*) = {
      new PointSegment(points map (x=> MapPoint(x))) 
    }
  }

  class MapRegion(val segmentId : Int, val name: String) {
  }

  trait Segment
  {
    val id : Int;
    val children: List[Segment];
    def equivalentTo(other: Segment) : Boolean
    def leafSegments : List[PointSegment]
    def splitByPointSegment(nextid: Int, subPointSegment: PointSegment) : Segment 
  }

  class GroupSegment(val id : Int, val children: List[Segment]) extends Segment
  { 
    def splitByPointSegment(nextid: Int, subPointSegment: PointSegment) : Segment = this; 
    def leafSegments = children.map(x => x.leafSegments).flatten
    def equivalentTo(other: Segment) = {
      false
    }
  }

  class PolyMap(val shapes: List[MapRegion], nextSegmentNum : Int = 1, val segments: List[Segment]){
    def this() = this(List(), 1, List())

      def addShape(pointSegment: PointSegment, name: String) : PolyMap = {
      val newPointSegment = pointSegment.assignId(nextSegmentNum)

        var segmentNum = nextSegmentNum + 1

      val longestSubSegment = leafSegments.foldLeft(Seq[MapPoint]())((currentLongest, x)=> {
          var longest = Utility.longestCommonSubstring(newPointSegment.points, x.points);
          if(longest.length > currentLongest.length) longest
          else currentLongest

        });
      val newSegments = if(longestSubSegment.length > 0){
        println(longestSubSegment.length)
        val newSegment = new PointSegment(segmentNum, longestSubSegment)
          segmentNum = segmentNum + 1
        segments.map(x=>x.splitByPointSegment(segmentNum, newSegment))
      }
      else{
        segments
      }
      new PolyMap(new MapRegion(nextSegmentNum, name)::shapes, segmentNum, newPointSegment :: newSegments)
    }

    val leafSegments : Seq[PointSegment] = {
      (for(segment <- segments)
        yield segment.leafSegments)
      .flatten.toSet.toSeq
    } 

    def getSegment(id: Int) = {
      segments.filter(x => x.id == id).head
    }
  }
}
