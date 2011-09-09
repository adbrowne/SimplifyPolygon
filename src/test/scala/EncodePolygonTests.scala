package net.hasnext.mapping {

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class EncodePolygonSpec extends FlatSpec with ShouldMatchers {
  "Double" should "encode" in {
    val initial = 38.5
    val result = PolygonEncode.encode(initial)
    result should equal("_p~iF")
  }
  "Integer" should "encode" in {
    val initial = -17998321
    val result = PolygonEncode.encode(initial)
    result should equal("`~oia@")
  }
  "Point" should "encode" in {
    val initial = new Point(38.5, -120.2)
    val result = PolygonEncode.encode(initial)
    result should equal("_p~iF~ps|U")
  }
  "List Point" should "encode" in {
    val initial = List(
      new Point(38.5, -120.2),
      new Point(40.7, -120.95),
      new Point(43.252, -126.453)
    )
    val result = PolygonEncode.encode(initial)
    result should equal("_p~iF~ps|U_ulLnnqC_mqNvxq`@")
  }
}

}
