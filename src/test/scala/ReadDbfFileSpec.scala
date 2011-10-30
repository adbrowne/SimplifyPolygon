import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.linuxense.javadbf._

package net.hasnext.mapping.tests {

  import java.io.{FileInputStream, InputStream}

  class DbfReader(fileName: String) {
    def getFieldNames(reader: DBFReader, index: Int, total: Int, fieldNames: List[String]) : List[String]= {
      if(index == total){
        fieldNames.reverse
      }
      else{
        val field : DBFField = reader.getField(index)
          val name = field.getName().trim() 
          println(name)
        getFieldNames(reader, index + 1, total, name :: fieldNames)
      }
    }
    def getFieldNames(reader: DBFReader) : List[String] = {
      val numberOfFields = reader.getFieldCount();
      getFieldNames(reader, 0, numberOfFields, List())
    }
    def getRows(reader: DBFReader, fieldNames: List[String]) : List[Map[String, Object]] = {
      var rows : List[Map[String, Object]] = List()
        var rowObjects : Array[Object] = Array()
        while( rowObjects != null) {
        var row : Map[String, Object] = Map()
          var rowIterator = 0;
        while( rowIterator< rowObjects.length) {
          val fieldName = fieldNames(rowIterator)
            val fieldValue = rowObjects(rowIterator)
            row += (fieldName -> fieldValue)
          rowIterator += 1;
        }
        if(rowObjects.length > 0){
          rows = row :: rows
        }
        rowObjects = reader.nextRecord()
      }
      rows.reverse
    }
    def read = {
      var inputStream = new FileInputStream(fileName);
      var reader = new DBFReader(inputStream);

      val fieldNames = getFieldNames(reader)

        getRows(reader, fieldNames)

    }
  }

  class ReadDbfFileSpec extends FlatSpec with ShouldMatchers {
    "LoadDBFFile" should "open file" in{
      val reader = new DbfReader("./data/australia/australia.dbf")
        for (row <- reader.read)
        println(row("ADMIN_NAME"))
    }
    "LoadDBFFile" should "open postcode file" in{
      val reader = new DbfReader("./data/aus_postcodes/POA06aAUST_region.dbf")
        for (row <- reader.read)
        println(row("POA_2006"))
    }
  }
}
