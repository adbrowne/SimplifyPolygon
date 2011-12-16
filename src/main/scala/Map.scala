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
  case class MapPoint(val x: Float, val y: Float)

  object MapPoint {
    def apply(point: Tuple2[Int,Int]) = {
      point match {
        case (x,y) => new MapPoint(x,y)
      }
    } 

  }
  class Segment(val points : Seq[MapPoint]){
    def this() = this(List())
    def splitBySegment(subsegment: Segment) : List[Segment] = {
      val index = points.indexOfSlice(subsegment.points)

      if(index == -1)
      {
        List(this)
      }
      else{
        val initialSequenceLength = index
        val initial = points.take(index)
        val subsegmentLength = subsegment.points.length
        val leftOver = new Segment(points.drop(initialSequenceLength + subsegmentLength))
       
        val rest = leftOver.points.length match {
          case 0 => List[Segment]()
          case _ => leftOver.splitBySegment(subsegment)
        }
        if(index > 0){
          new Segment(initial) :: subsegment :: rest
        }
        else{
          subsegment :: rest
        }
      }
    }
    override def equals(other: Any) = other match {
      case that: Segment =>
        this.points == that.points
      case _ =>
        false
    }
    override def toString = {
        "Segment(" + points.toString + ")"
    }
  }

  object Segment {
    def apply(points: Tuple2[Int,Int]*) = {
      new Segment(points map (x=> MapPoint(x))) 
    }
  }
  class MapRegion(val segments : List[Segment]) {
    def findCommonSegments(otherRegion: MapRegion) = {
      val extract = Utility.longestCommonSubstring(segments.head.points, otherRegion.segments.head.points)    
      extract match {
        case Seq() => this;
        case xs => new MapRegion(List(new Segment(extract)))
      }
    }
    def this() = this(List())
  }

  object MapRegion{
    def apply(points: Tuple2[Int,Int]*) = {
      // TODO: Fix the duplication here. Should be able to call the segment singleton constructor
      new MapRegion(List(new Segment(points map (x => MapPoint(x)))))
    }
  }
  class PolyMap(val shapes: List[MapRegion]){
    def this() = this(List())
      def addShape(shape: MapRegion) : PolyMap = {
      new PolyMap(shape::shapes)
    }
  }
}
