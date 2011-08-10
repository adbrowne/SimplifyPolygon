case class Stuff(mappoints: List[List[List[Double]]])
object SimplifyJson{
  def simplifyJson(inputFile : String, outputFile: String, epsilon: Double){
    import net.liftweb.json.JsonAST._
    import net.liftweb.json.Extraction._
    import net.liftweb.json.Printer._

    val source = scala.io.Source.fromFile(inputFile)
    val fileLines = source.mkString
    source.close

    implicit val formats = net.liftweb.json.DefaultFormats

    var parsed = net.liftweb.json.parse(fileLines)

    var stuff = parsed.extract[Stuff]

    val lines = stuff.mappoints.map(_.map( point => new Point(point.head, point.last)))

      val line = lines head
      var firstPoint = line head

    val simplified = lines.map(Simplify.simplify(_,epsilon))
    //simplified.length should equal(84)

    val outputStuff = new Stuff(mappoints = simplified.map(_.map((point) => List(point.x,point.y))))

    val outputString = pretty(render(decompose(outputStuff)))

    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
          try { op(p) } finally { p.close() }
      }
      printToFile(new java.io.File(outputFile))(p => {
            p.println(outputString)
          })
  }
}
