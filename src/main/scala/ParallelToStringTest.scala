
/*package net.hasnext.mapping{
  object ParallelToStringTest extends App {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
        try { op(p) } finally { p.close() }
    }
    def doubleToString(x: Double) : String = {
      """{
          "X":""" + x + """
        }""" toString
    }

    val randomGen = new scala.util.Random()

      val numbers = (0 until 1000000).map((x => x.toDouble))
    Console.println("Press enter to start")
    Console.readLine
    val startTime = System.currentTimeMillis()
      numbers.map(doubleToString)
    val endTime = System.currentTimeMillis()
    Console.println(endTime - startTime)
    }
}*/
