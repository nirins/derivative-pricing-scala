package Entity

import java.util._
import java.text._

case class DateValuePair(
  var date: Date,
  var value: Double    
){  
}

object DateValuePair{
  def FromCsv(csv: String): DateValuePair = {
    val a = csv.split(",");
    val format = new SimpleDateFormat("yyyy-MM-dd");
    val d = format.parse(a(0));
    val v = if (a(1)==".") 0 else a(1).toDouble;
    return new DateValuePair(d,v);
  }  
}