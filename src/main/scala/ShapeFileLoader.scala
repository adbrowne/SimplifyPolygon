package net.hasnext.mapping{
  import java.io.File
  import java.io.FileInputStream
  import java.nio.channels.FileChannel.MapMode._
  import java.nio.ByteOrder._
  
class Part(val points: List[Point])
case class BoundingBox(val min: Point, val max: Point)


object ShapeFileLoader{
  def readPoint(startIndex: Int, buffer: java.nio.ByteBuffer) = {
    new Point(buffer.getDouble(startIndex), buffer.getDouble(startIndex + 8))
  }

  def actionFile(fileName: String, actionShape: (Int, Shape) => Unit) {
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

      var currentOffset = 100
    /*    import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
    val dispatcher = akka.dispatch.Dispatchers.newExecutorBasedEventDrivenDispatcher("name")
      val built =  dispatcher
    .withNewThreadPoolWithLinkedBlockingQueueWithCapacity(100)
    .setCorePoolSize(16)
    .setMaxPoolSize(1000)
    .setKeepAliveTimeInMillis(60000)
    .setRejectionPolicy(new CallerRunsPolicy)
    .build */

    while(currentOffset < fileLength*2){
      val shapeContentLength = readAndActionShape(buffer,currentOffset,actionShape)
        currentOffset = currentOffset + shapeContentLength * 2 + 8
    }


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

      var shapes = scala.collection.mutable.ArraySeq[Shape]()
      var currentOffset = 100
    while(currentOffset < fileLength*2){
      val shape = readShape(buffer,currentOffset)
        shapes = shapes :+ shape
      currentOffset = currentOffset + shape.contentLength * 2 + 8
    }
    new ShapeFile(fileLength, version, new BoundingBox(min, max), shapeType, shapes.toList) 

  }

  private def readAndActionShape(buffer: java.nio.ByteBuffer,  offset: Int,actionShape: (Int, Shape) => Unit) = {
    import scala.collection.mutable
    buffer.order(BIG_ENDIAN)
    val recordNumber = buffer.getInt(offset)
      val contentLength = buffer.getInt(offset + 4)
      buffer.order(LITTLE_ENDIAN) 
    val polygonStart = offset + 8
    val min = readPoint(polygonStart + 4, buffer)
      val max = readPoint(polygonStart + 20, buffer)

      val partCount = buffer.getInt(polygonStart + 36)
      val pointCount = buffer.getInt(polygonStart + 40)
      val partIndexOffset = polygonStart + 44
    var pointIndexes = new mutable.ArraySeq[Int](partCount)
      for(i <- 0 until partCount){
      val partStartIndex = buffer.getInt(partIndexOffset + 4 * i)
        pointIndexes.update(i, partStartIndex)
    }

    //var points = mutable.ArraySeq[Point]()
    val pointIndexOffset = partIndexOffset + partCount * 4
    //println("partIndexOffset: " + partIndexOffset)
    //println("pointIndexOffset: " + pointIndexOffset)
    val points = (0 until pointCount).map((i: Int) => readPoint(pointIndexOffset + 16 * i, buffer))


      val shape =   new Shape(recordNumber, contentLength, new BoundingBox(min, max), partCount, pointCount, getParts(pointIndexes, points)) 
      actionShape(shape.recordNumber, shape) 

    //actionShape(shape.recordNumber, shape) 

    contentLength
  }

  private def readShape(buffer: java.nio.ByteBuffer,  offset: Int) = {
    import scala.collection.mutable
    buffer.order(BIG_ENDIAN)
    val recordNumber = buffer.getInt(offset)
      val contentLength = buffer.getInt(offset + 4)
      buffer.order(LITTLE_ENDIAN)
    val polygonStart = offset + 8
    val min = readPoint(polygonStart + 4, buffer)
      val max = readPoint(polygonStart + 20, buffer)

      val partCount = buffer.getInt(polygonStart + 36)
      val pointCount = buffer.getInt(polygonStart + 40)
      val partIndexOffset = polygonStart + 44
    var pointIndexes = new mutable.ArraySeq[Int](partCount)
      for(i <- 0 until partCount){
      val partStartIndex = buffer.getInt(partIndexOffset + 4 * i)
        pointIndexes.update(i, partStartIndex)
    }

    //var points = mutable.ArraySeq[Point]()
    val pointIndexOffset = partIndexOffset + partCount * 4
    //println("partIndexOffset: " + partIndexOffset)
    //println("pointIndexOffset: " + pointIndexOffset)
    val points = (0 until pointCount).map((i: Int) => readPoint(pointIndexOffset + 16 * i, buffer))
      new Shape(recordNumber, contentLength, new BoundingBox(min, max), partCount, pointCount, getParts(pointIndexes, points)) 
  }

  import scala.collection.mutable
  def getParts(pointIndexes: mutable.ArraySeq[Int], points: Seq[Point]) = {
    var parts = mutable.ArraySeq[Part]()

      val partCount = pointIndexes.length
    val pointCount = points.length
    for(i <- 0 until partCount){
      val startIndex = pointIndexes(i)
        val endIndex = if(i == partCount - 1) pointCount else pointIndexes(i+1)

        val partPoints = points.drop(startIndex).take(endIndex - startIndex)
        parts = parts :+ new Part(partPoints.toList)
    }

    parts.toList

  }
}
}
