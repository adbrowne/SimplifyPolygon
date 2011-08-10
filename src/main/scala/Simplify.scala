object Simplify{
  def maxDistance(lineStart: Point, lineEnd: Point, points: List[Point]) = {
    val distances = (points map(_.distanceTo(lineStart, lineEnd))) toArray
    var max = 0.0
    var maxIndex = 0

    for(i <- 0 until distances.length){
      if(distances(i) > max){
        max = distances(i)
        maxIndex = i
      }
    }
    (max, maxIndex)
  }

  // http://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
  def simplify(line: List[Point], epsilon: Double) = {
    val linePoints = line.length
    if(line.length <= 2)
      line
    else{
      var lineStart = line.head
      var lineEnd = line.last
      var midPoints = line.tail.reverse.tail.reverse

      var max = maxDistance(lineStart, lineEnd, midPoints);

      if(max._1 > epsilon){
        line 
      }
      else{
        List(lineStart, lineEnd) 
      }
    }
  }
}
