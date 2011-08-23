package net.hasnext.mapping{
  object Simplify{
    def maxDistance(line: List[Point]) = {
      var lineStart = line.head
      var lineEnd = line.last

      val distances = (line map(_.distanceTo(lineStart, lineEnd))) toArray
      var max = 0.0
      var maxIndex = 0

      for(i <- 1 until distances.length - 1){
        if(distances(i) > max){
          max = distances(i)
          maxIndex = i
        }
      }
      (max, maxIndex)
    }

    def divideLine(line: List[Point], midPointIndex: Int) = {
      val splitIndex = midPointIndex
      val (beforeMidpoint, rest) = line splitAt splitIndex
      val midPoint :: afterMidpoint = rest
      (beforeMidpoint ::: List(midPoint), midPoint ::  afterMidpoint)
    }

    // http://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
    def simplify(line: List[Point], epsilon: Double) : List[Point] = {
      val linePoints = line.length
      if(line.length <= 2)
        line
      else{
        val (maxDistanceAmount, maxDistanceIndex) = maxDistance(line);
        if(maxDistanceAmount > epsilon){
          val (segment1, segment2) = divideLine(line, maxDistanceIndex)
            val simple1 = simplify(segment1,epsilon)
            val midPoint :: simple2 = simplify(segment2,epsilon)
            simple1 ++ simple2
        }
        else{
          line.head :: line.last :: Nil
        }
      }
    }
  }
}
