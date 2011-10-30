import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.linuxense.javadbf._

package net.hasnext.mapping.tests {

  import java.io.{FileInputStream, InputStream}
  import net.hasnext.mapping.DbfReader

  class ReadDbfFileSpec extends FlatSpec with ShouldMatchers {
    "LoadDBFFile" should "open file" in{
      val reader = new DbfReader("./data/australia/australia.dbf")
        for (row <- reader.read)
        println(row("ADMIN_NAME"))
    }
    "LoadDBFFile" should "open postcode file" in{
      val reader = new DbfReader("./data/aus_postcodes/POA06aAUST_region.dbf")
      reader.read
    }
  }
}
