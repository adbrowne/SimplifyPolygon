package net.hasnext.mapping{
  class ShapeFile(
      val fileLength: Int, 
      val version: Int, 
      val boundingBox: BoundingBox,
      val shapeType: Int, 
      val shapes: List[Shape]
      )
}
