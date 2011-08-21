package net.hasnext.mapping{
  object ProcessFile extends App {
    def printToFile(fileName: String)(op: java.io.BufferedOutputStream => Unit) {
      val bufferedOutput = new java.io.BufferedOutputStream(new java.io.FileOutputStream(fileName))
        try { op(bufferedOutput) } finally { bufferedOutput.flush();bufferedOutput.close() }
    }
    def pointToString(point: Point, output: String => Unit) = {
      output("""{
          "X":""")
          output(point.X.toString)
          output(""",
            "Y":""") 
          output(point.Y.toString)
          output( """
          }""" )
      }
      def partToString(part: Part, output: String => Unit) = {
        output("""{
            "points":[""") 

          var first = true
          part.points.foreach(
            s => {
              if(!first) output(",")
                pointToString(s,output) 
              first = false
            })
          output("]}")
        }
        def shapeToString(shape: Shape, output: String => Unit) = {
          output ("""{ "recordNumber":""" + shape.recordNumber + """
            ,
            "parts":[""")

          var first = true
          shape.parts.foreach(
            s => {
              if(!first) output(",")
                partToString(s,output) 
              first = false
            })

          output ("]}")
        }
        import net.liftweb.json.JsonAST._
        import net.liftweb.json.Extraction._
        import net.liftweb.json.Printer._
        implicit val formats = net.liftweb.json.DefaultFormats
        
        def writeString(stream: java.io.BufferedOutputStream)(string: String) {
          stream.write(string.getBytes())
        }
        def writeFile(recordNum: Int, shape: Shape) {
          //val outputString = shapeToString(shape)

          val outputFile = "./map/postcodes/"+recordNum+".js"

          printToFile(outputFile)(p => {
              shapeToString(shape, writeString(p))
              //p.println(outputString)
            })

        }
        Console.println("Press enter to start2")
        Console.readLine
        val startTime = System.currentTimeMillis()
          val shapeFile = ShapeFileLoader.actionFile("../erl-shapelib/aus_postcodes/POA06aAUST_region.shp", writeFile)
          val endTime = System.currentTimeMillis()
          Console.println(endTime - startTime)
      }
    }
