package net.hasnext.mapping{
  object ProcessFile extends App {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
        try { op(p) } finally { p.close() }
    }
    def pointToString(point: Point, output: String => Unit) = {
      output("""{
          "X":""")
        output(point.x.toString)
        output(""",
          "Y":""") 
        output(point.y.toString)
        output( """
        }""" )
    }
    def partToEncodedString(part: Part, output: String => Unit) = {
      val points = Simplify.simplify(part.points,0.05)
      val result = PolygonEncode.encode(points)
      val encodedResult = result.replace("\\", "\\\\")
        output("""{"points":""") 
          output("\"")
          output(encodedResult)
          output("\"")
          output("}")
    }

    def partToString(part: Part, output: String => Unit) = {
      val points = Simplify.simplify(part.points,0.05)
        output("""{
            "points":[""") 

          var first = true
          points.foreach(
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
              partToEncodedString(s,output) 
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

        val outputFile = "./map/postcodese/"+recordNum+".js"
        printToFile(new java.io.File(outputFile))(p => {
            shapeToString(shape, p.print)
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
