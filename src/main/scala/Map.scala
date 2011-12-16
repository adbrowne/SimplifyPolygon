package net.hasnext.mapping{
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
    override def equals(other: Any) = other match {
      case that: Segment =>
        this.points == that.points
      case _ =>
        false
    }
  }

  object Segment {
    def apply(points: Tuple2[Int,Int]*) = {
      new Segment(points map (x=> MapPoint(x))) 
    }
  }
  class MapRegion(val segments : List[Segment]) {
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
