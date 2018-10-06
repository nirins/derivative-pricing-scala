package Swap

import scala.io.Source._
import scala.collection.mutable._
import Entity._

object LIBORRate {

  def Get1MonthLIBOR(): List[DateValuePair] = {
    val filepath = "Resource/USD1MTD156N.csv";
    val lines = fromFile(filepath).getLines;

    var values = new MutableList[DateValuePair]();
    for (line <- lines.drop(1)) {
      val dp = DateValuePair.FromCsv(line);
      values += dp;
    }
    return values.toList;
  }
}