import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack


class JsonSpec extends FlatSpec with ShouldMatchers {
  "A json file" should "be loadable" in{ 
    val source = scala.io.Source.fromFile("/home/adb/public_html/map/mapdata/1.js")
      val lines = source.mkString
    source.close
  }
  "A json file" should "can be parsed" in{ 
    val source = scala.io.Source.fromFile("/home/adb/public_html/map/mapdata/1.js")
      val lines = source.mkString
    source.close

    implicit val formats = net.liftweb.json.DefaultFormats

    var parsed = net.liftweb.json.parse(lines)

      var stuff = parsed.extract[Stuff]

    stuff.mappoints.head.head.head should equal (141.00265502929688)
  }
  "A json file" should "can simplified" in{ 
    for(index <- 0 until 8)
      SimplifyJson.simplifyJson("/home/adb/public_html/map/mapdata/"+ index + ".js","/home/adb/public_html/map/mapdata/" + index + "_simple.js",0.05)
  }
}
