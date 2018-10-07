package Swap

import scala.io.Source._
import scala.collection._
import java.text._
import java.util._
import java.time._
import java.time.format._
import Entity._

object LIBORRate {

  def loadCsvFile(filepath: String): Seq[(LocalDate, Double)] = {
    val lines = fromFile(filepath).getLines;
    val df = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    var values = new mutable.MutableList[(LocalDate, Double)]();
    for (line <- lines.drop(1)) {
      val a = line.split(",");
      val d = LocalDate.parse(a(0), df);
      val v = if (a(1)==".") 0 else a(1).toDouble/100;           
      values += d -> v;
    }
    return values.toList.toSeq;    
  }
  
  def getLIBOR1M(): Seq[(LocalDate, Double)] = {
    val filepath = "Resource/USD1MTD156N.csv";
    return loadCsvFile(filepath);
  }
  
  def getLIBOR3M(): Seq[(LocalDate, Double)] = {
    val filepath = "Resource/USD3MTD156N.csv";
    return loadCsvFile(filepath);
  }

    
  def getLIBOR6M(): Seq[(LocalDate, Double)] = {
    val filepath = "Resource/USD6MTD156N.csv";
    return loadCsvFile(filepath);
  }

  def getLIBOR9M(): Seq[(LocalDate, Double)] = {
    val filepath = "Resource/USD9MTD156N.csv";
    return loadCsvFile(filepath);
  }  
  
  def getLIBOR12M(): Seq[(LocalDate, Double)] = {
    val filepath = "Resource/USD12MD156N.csv";
    return loadCsvFile(filepath);
  }

}