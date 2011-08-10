package net.hasnext.mapping{
  import java.io.File
  import java.io.FileInputStream
  import java.nio.channels.FileChannel.MapMode._
  import java.nio.ByteOrder._
  class ShapeFile(val fileLength: Int, val version: Int, val boundingBox: BoundingBox, val shapeType: Int){

  }
  case class Point(val X: Double, val Y: Double)
  case class BoundingBox(val min: Point, val max: Point)
  

  object ShapeFileLoader{
    def readPoint(startIndex: Int, buffer: java.nio.ByteBuffer) = {
      new Point(buffer.getDouble(startIndex), buffer.getDouble(startIndex + 8))
    }
    def readFile(fileName: String) = {
      val file = new File(fileName)
      val fileSize = file.length
      val stream = new FileInputStream(file)
      val buffer = stream.getChannel.map(READ_ONLY, 0, fileSize)

      buffer.order(BIG_ENDIAN)
      val fileLength = buffer.getInt(24)
      
      buffer.order(LITTLE_ENDIAN)
      val version = buffer.getInt(28)
      val shapeType = buffer.getInt(32)
      val min = readPoint(36, buffer)
      val max = readPoint(52, buffer)
      new ShapeFile(fileLength, version, new BoundingBox(min, max), shapeType)

    }
  }
}
