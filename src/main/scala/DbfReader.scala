import com.linuxense.javadbf._

package net.hasnext.mapping {

  import java.io.{FileInputStream, InputStream}

  class DbfReader(fileName: String) {
    class ReaderImpl(reader : DBFReader){
      val fieldNames = getFieldNames()
    def getFieldName(
      index: Int) = {
        val field : DBFField = reader.getField(index)
        field.getName().trim() 
    }
    def getFieldNames(
      index: Int, 
      total: Int, 
      fieldNames: List[String]) 
    : List[String]= {
      if(index == total){
        fieldNames.reverse
      }
      else{
        val name = getFieldName(index)
        getFieldNames(index + 1, total, name :: fieldNames)
      }
    }

    def getFieldNames() : List[String] = {
      val numberOfFields = reader.getFieldCount();
      getFieldNames(0, numberOfFields, List())
    }

    def getRows() : List[Map[String, Object]] = {
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


        getRows()
    }
    }
    def read = {
      var inputStream = new FileInputStream(fileName);
      var reader = new DBFReader(inputStream);
      (new ReaderImpl(reader)).read
    }
  }
}
