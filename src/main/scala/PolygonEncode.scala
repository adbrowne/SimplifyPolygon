package net.hasnext.mapping{
  object PolygonEncode {
    def encodeSignedNumber(num: Int) = { 
      val sgn_num = num << 1; 
      val numberToEncode = if (num < 0) { 
        ~(sgn_num); 
      }
      else{
        sgn_num
      }
      (encodeNumber(numberToEncode)); 
    } 
    def encodeNumber(num: Int) = { 
      var encodeString = ""; 
      var nextValue : Int = 0; 
      var number = num 
      while (number >= 0x20) { 
        nextValue = (0x20 | (number & 0x1f)) + 63; 
        if (nextValue == 92) { 
          encodeString += nextValue.toChar; 
        } 
        encodeString += nextValue.toChar; 
        number = number >> 5; 
      } 
      var finalValue = number + 63; 
      if (finalValue == 92) { 
        encodeString += finalValue.toChar; 
      } 
      encodeString += finalValue.toChar; 
      encodeString; 
    } 
    def e5(num: Double) : Int = {
      (num * scala.math.pow(10,5)).toInt
    }
    def encode(num: Double) : String = {
      val integer = (num * scala.math.pow(10,5)).toInt
      encodeSignedNumber(integer)
    }
    def encode(num: Int) : String = {
      encodeSignedNumber(num)
    }
    def encode(point: Point) : String = {
      var encodedX = encode(point.x)
        val encodedY = encode(point.y)
        encodedX + encodedY
    }
    def encode(points: List[Point]) : String = {
      val sb = new StringBuilder()

      var lastPoint = new Point(0,0)
      for(point <- points){
        var encodedX = encode(e5(point.x) - e5(lastPoint.x))
        val encodedY = encode(e5(point.y) - e5(lastPoint.y))
        sb.append(encodedX)
        sb.append(encodedY)
        lastPoint = point
      }
      sb.toString
    }
  }
}
