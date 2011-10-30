package net.hasnext.mapping{
  object ProcessFile extends App {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
        try { op(p) } finally { p.close() }
    }
    def pointToString(point: Point, output: String => Unit) = {
      output("""{
          "X":""")
        output("%1.2f" format point.x)
        output(""",
          "Y":""") 
        output("%1.2f" format point.y)
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

    def partToString(startComma: Boolean, part: Part, output: String => Unit) = {
      val points = Simplify.simplify(part.points,0.05)
        if(points.length > 3){
          if(startComma) output(",")
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
          true
        }
        else{
          false
        }
      }
      def shapeToString(shape: Shape, output: String => Unit, recordData: Map[String,Object]) = {
        output ("""{ "recordNumber":""" + shape.recordNumber + """
          "name":""" + recordData("POA_2006") + """ 
          ,
          "parts":[""")

        var first = true
        shape.parts.foreach(
          s => {
            val printed = partToString(!first, s,output) 
              if(printed)
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
      def writeShape(p: java.io.PrintWriter, recordData: List[Map[String, Object]])(recordNum: Int, shape: Shape){
        if(recordNum > 1)
          p.print(",")

        shapeToString(shape, p.print, recordData(recordNum - 1))
      }
      Console.println("Press enter to start2")
      Console.readLine
      val file = new java.io.File("./map/all_0_05.js")
        val p = new java.io.PrintWriter(file)
        try { 
        p.print("""{"shapes":[""")
        val dbfFileName = "./data/aus_postcodes/POA06aAUST_region.dbf"
        val recordData = (new DbfReader(dbfFileName).read)
        val shapeAction = writeShape(p, recordData)_
        val startTime = System.currentTimeMillis()
          val inputFileName ="./data/aus_postcodes/POA06aAUST_region.shp"
        val shapeFile = ShapeFileLoader.actionFile(inputFileName, shapeAction)
          val endTime = System.currentTimeMillis()
          p.print("""]}""")
        Console.println(endTime - startTime)
      }
      finally { 
        p.close() 
      }
    }
  }
