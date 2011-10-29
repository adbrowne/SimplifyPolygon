import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.linuxense.javadbf._

package net.hasnext.mapping.tests {

import java.io.{FileInputStream, InputStream}

class ReadDbfFileSpec extends FlatSpec with ShouldMatchers {
  "LoadDBFFile" should "open file" in{
      var inputStream = new FileInputStream( "./data/australia/australia.dbf");
       var reader = new DBFReader( inputStream);

      // get the field count if you want for some reasons like the following
      //
      val numberOfFields = reader.getFieldCount();
    var i = 0;
    while(i< numberOfFields){
        val field : DBFField = reader.getField( i);

        // do something with it if you want
        // refer the JavaDoc API reference for more details
        //
        System.out.println( field.getName());
      i+=1;
    }
       var rowObjects : Array[Object] = Array()
     while( rowObjects != null) {

       var rowIterator = 0;
        while( rowIterator< rowObjects.length) {

          System.out.println( rowObjects(rowIterator));
          rowIterator += 1;
        }
       rowObjects = reader.nextRecord()
      }
    println(numberOfFields);
  }
 }
}