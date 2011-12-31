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
  
    val tolerance = 0.00001

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
  
  class PointSegment(val points : Seq[MapPoint]){
    def this() = this(List())
    def splitByPointSegment(subPointSegment: PointSegment) : List[PointSegment] = {
      val index = points.indexOfSlice(subPointSegment.points)

      if(index == -1)
      {
        List(this)
      }
      else{
        val initialSequenceLength = index
        val initial = points.take(index)
        val subPointSegmentLength = subPointSegment.points.length
        val leftOver = new PointSegment(points.drop(initialSequenceLength + subPointSegmentLength))
       
        val rest = leftOver.points.length match {
          case 0 => List[PointSegment]()
          case _ => leftOver.splitByPointSegment(subPointSegment)
        }
        if(index > 0){
          new PointSegment(initial) :: subPointSegment :: rest
        }
        else{
          subPointSegment :: rest
        }
      }
    }
    def equivalentTo(other: PointSegment) = {
      this.points == other.points
    }
    override def equals(other: Any) = {
      throw new Exception() 
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

  class MapRegion(val segments : List[PointSegment]) {
    def findCommonPointSegments(otherRegion: MapRegion) = {
      val extract = Utility.longestCommonSubstring(segments.head.points, otherRegion.segments.head.points)    
      extract match {
        case Seq() => this;
        case xs => new MapRegion(
          segments.map(x => x.splitByPointSegment(new PointSegment(extract))).flatten
        )
      }
    }
    def this() = this(List())
  }

  object MapRegion{
    def apply(points: Tuple2[Double,Double]*) = {
      // TODO: Fix the duplication here. Should be able to call the PointSegment singleton constructor
      new MapRegion(List(new PointSegment(points map (x => MapPoint(x)))))
    }
  }
  
  class PolyMap(pShapes: List[MapRegion]){
    val shapes : List[MapRegion] = {
      for(shape1 <- pShapes)
        yield pShapes.foldLeft(shape1)(
          (z,x) => 
          {
            if(z != x){
              z.findCommonPointSegments(x)
            }
            else{
              z
            }
          }
        )
    }
    
    def this() = this(List())
    
    def addShape(shape: MapRegion) : PolyMap = {
      new PolyMap(shape::shapes)
    }

    def segments = {
      (for(shape <- shapes)
        yield shape.segments).flatten.toSet.toSeq
    }
  }
}
