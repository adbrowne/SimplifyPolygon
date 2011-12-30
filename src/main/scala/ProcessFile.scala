package net.hasnext.mapping.js{
  case class Point(x: Float, y: Float);
  case class Segment(id: Int, points: Seq[Point])
  case class Region(name: String, segments: Seq[Int])
  case class Map(segments: Seq[Segment], regions: Seq[Region])
}
package net.hasnext.mapping{
  object ProcessFile extends App {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
        try { op(p) } finally { p.close() }
    }
    def pointToString(point: Point) = {

      """{
        "X":""" + 
        ("%1.2f" format point.x) + 
        (""",
          "Y":""")  + 
        ("%1.2f" format point.y) + 
        ( """
        }""" )

    }    
    def outputPoint(point: Point, output: String => Unit) = {
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
      val points = Simplify.simplify(part.points,0.1)
        if(points.length > 3){
        if(startComma) output(",")
          output("""{
            "points":[""") 

          var first = true
          points.foreach(
            s => {
              if(!first) output(",")
                outputPoint(s,output) 
              first = false
            })
          output("]}")
          true
        }
        else{
          false
        }
      }
      def shapeToScalaList(shape: Shape, output: String => Unit, postcode: Object) = {
        shape.parts.foreach(
          s => {
            output("\n" + postcode.toString)
            output(":\n")
            s.points.foreach(
              p => {
                output("(" + p.x + "," + p.y + ")")
              }
            )
            
        })
    }


    def shapeToString(shape: Shape, output: String => Unit, recordData: Map[String,Object]) = {
      output ("""{ "recordNumber":""" + shape.recordNumber + """
        ,
        "name":""" + '"' + recordData("POA_2006") + """"
        ,
        "center":""" + pointToString(shape.center) + """ 
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
    def getMapRegion(shape: Shape) = {
      
    }
    def writeBendigo(p: java.io.PrintWriter, recordData: List[Map[String, Object]])(recordNum: Int, shape: Shape){
      var postcode = recordData(recordNum - 1)("POA_2006");
      if(postcode == "3551" || postcode == "3550")
      {

      }
        shapeToScalaList(shape, p.print, postcode);
    }
  
    def addShapeToMap(shape: Shape, myMap: PolyMap) = {
      println(shape.parts.length);
      var map = myMap
      shape.parts.foreach(p => {
          map = map.addShape(
        new MapRegion(List(new Segment(p.points.map(x => {
          new MapPoint(x.x.toFloat,x.y.toFloat) 
          })))))
        });

      map
    }

    val myShapes = ShapeFileLoader.readFile("data/aus_postcodes/POA06aAUST_region.shp")

    
    var map = new PolyMap()

    map = addShapeToMap(myShapes.shapes(1),map)
    map = addShapeToMap(myShapes.shapes(2),map)
    map = addShapeToMap(myShapes.shapes(3),map)
    

    val segmentMap : Map[Segment,Int] = map.segments.zipWithIndex.map(x => {
        x match {
          case (segment, index) => (segment -> index)
        }
      }).toMap;

    val segments = segmentMap.map(x => {
        x match {
          case (segment, index) => 
            new net.hasnext.mapping.js.Segment(index, 
              //Simplify.simplify(segment.points,0.001).map(p => new net.hasnext.mapping.js.Point(p.x,p.y)) 
              segment.points.map(p => new net.hasnext.mapping.js.Point(p.x,p.y)) 
            )
        }
    }).toSeq;

    val regions = map.shapes.map(x => {
        var segmentIds = x.segments.map(s => 
          {
            segmentMap(s)
          });

      new net.hasnext.mapping.js.Region("RegionName",segmentIds)
    });

    val jsMap = new net.hasnext.mapping.js.Map(segments,regions)
    val stringOut = net.liftweb.json.Serialization.write(jsMap)

    val file = new java.io.File("./map/lineEncoded/lineencoded.js")
    val p = new java.io.PrintWriter(file)
    try { 
      p.print(stringOut)
    }
    finally { 
      p.close() 
    }
  }
}
