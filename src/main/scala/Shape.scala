package net.hasnext.mapping{
  class Shape(
      val recordNumber: Int, 
      val contentLength: Int, 
      val boundingBox: BoundingBox,
      val partCount: Int,
      val pointCount: Int,
      val parts: List[Part])
  {
    def center = {
      val allPoints = (parts map (p => p.points)).flatten
        val firstPoint = allPoints head
        val mostLeft = allPoints.foldLeft(firstPoint)((p1, p2) => if(p1.x < p2.x){  p1 } else{  p2})
        val mostRight = allPoints.foldLeft(firstPoint)((p1, p2) => if(p1.x > p2.x){  p1 } else{  p2})
        val mostTop = allPoints.foldLeft(firstPoint)((p1, p2) => if(p1.y < p2.y){  p1 } else{  p2})
        val mostBottom = allPoints.foldLeft(firstPoint)((p1, p2) => if(p1.y > p2.y){  p1 } else{  p2})
        val centerX = mostLeft.x + (mostRight.x - mostLeft.x) / 2
        val centerY = mostBottom.y + (mostTop.x - mostBottom.x) / 2
        new Point(centerX,centerY)
    }
  }
}
