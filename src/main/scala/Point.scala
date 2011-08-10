class Point(val x: Double, val y: Double){
  def distanceTo(other: Point) = {
    val xDif = this.x - other.x
    val yDif = this.y - other.y

    math.sqrt(xDif * xDif + yDif * yDif)
  }

  // http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
  def distanceTo(lineStart: Point, lineEnd: Point) = {
    val numerator = math.abs(
      (lineEnd.x - lineStart.x) * (lineStart.y - this.y) 
      - 
      (lineStart.x - this.x) * (lineEnd.y - lineStart.y) 
    )
  
    val denominator = math.sqrt( math.pow((lineEnd.x - lineStart.x),2) + math.pow((lineEnd.y - lineStart.y),2))
    numerator / denominator
  }

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
