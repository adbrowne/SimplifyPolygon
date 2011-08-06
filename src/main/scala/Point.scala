class Point(val x: Double, val y: Double){
  def distanceTo(other: Point) = {
    val xDif = this.x - other.x
    val yDif = this.y - other.y

    math.sqrt(xDif * xDif + yDif * yDif)
  }
}
