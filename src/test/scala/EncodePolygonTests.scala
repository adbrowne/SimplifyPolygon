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
  /*
  "Sydney" should "encode" in {
    val initial = List(
      new Point(-34.59553987981164, 151.09722811874997),
      new Point(-32.41552637952531, 148.63629061874997),
      new Point(-31.894665688451134, 152.96490389999997) ,
    new Point(-34.55935707696108, 153.53619296249997)
    )
    val result = PolygonEncode.encode(initial)
    result should equal("b}crEudfy[axhLzs_NkvdByllYhmgOqqnB")
  } */
}

}
