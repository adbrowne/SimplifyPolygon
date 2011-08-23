package net.hasnext.mapping{
  class Point(val x: Double, val y: Double){
    def distanceTo(other: Point) : Double = {
      val xDif = this.x - other.x
      val yDif = this.y - other.y

      math.sqrt(xDif * xDif + yDif * yDif)
    }

    // http://paulbourke.net/geometry/pointline/
    def distanceTo(lineStart: Point, lineEnd: Point) : Double = {
      def calculateU(x1:Double,y1:Double,x2:Double,y2:Double,x3:Double,y3:Double) = {
        val numerator = (x3 -x1) * (x2 - x1) + (y3 - y1)* (y2 - y1)
          val denominator = math.pow(lineStart distanceTo lineEnd, 2)

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

          val linePoint = new Point(linePointX, linePointY)

          this.distanceTo(linePoint)
      }
      // Line point was outside of the ends of the provided segment so just find the distance to the nearest end
      else{
        math.min(this.distanceTo(lineStart), this.distanceTo(lineEnd))
      }
    }

    override def toString = "(" + x + "," + y + ")"
    override def hashCode = 41 * (41 + x.toInt) + y.toInt

    override def equals(other: Any) = other match {
      case that: Point =>
      (that canEqual this) &&
      (this.x == that.x) && (this.y == that.y)
      case _ =>
      false
    }
    def canEqual(other: Any) = other.isInstanceOf[Point]

  }
}
