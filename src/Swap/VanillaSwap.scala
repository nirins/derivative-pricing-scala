package Swap

import scala.collection.immutable._
import java.util.Date
import java.text._
import java.time._
import java.time.format._

import Entity._

class VanillaSwap {
  var libor1m = Seq[(String, Double)]();
  var libor1m_norm = Seq[(String, Double)]();  
  
  def contructLIBORCurve() = {
    val libor1m_byDay = LIBORRate.Get1MonthLIBOR();
    val df = new SimpleDateFormat("yyyyMM");
    val groups: Map[String, Double] = libor1m_byDay.groupBy(x => df.format(x.date)).mapValues(libor1m_byDay => libor1m_byDay.map(_.value).sum / libor1m_byDay.size);
    libor1m = groups.toList.toSeq.sortBy(t => t._1);    
    libor1m_norm = libor1m.map(t => (t._1, t._2/100.0 + 1)).toList.toSeq;
  }
 
  def generateSwapCurve(maturity: Integer, from_date: LocalDate, to_date: LocalDate): Seq[(String, Double)] = {
    var current_date = from_date;
    val df = DateTimeFormatter.ofPattern("yyyyMM");
    
    var swap_curve = new scala.collection.mutable.MutableList[(String, Double)]();
    
    while(current_date.compareTo(to_date) < 0){
      var expected_floating_rate = libor1m.filter(t => t._1 > current_date.format(df) && t._1 <= current_date.plusYears(maturity.toLong).format(df)).map(t => t._2);
      var p = calcFairFixRate(maturity, expected_floating_rate);
      swap_curve += df.format(current_date) -> p;      
      current_date = current_date.plusMonths(1);
    }
    
    return swap_curve.toList.toSeq;
  }
  
  def calcFairFixRate(maturity: Int, expected_floating_rate: Seq[Double]): Double = {
      var frequency = 12;
      var N = maturity * frequency;

      var last = expected_floating_rate(N - 1) / frequency;
      var fixed_rate = 100 - 100 / math.pow( 1 + last/100, N);

      var denominator = 0.0;
      for (i <- 0 until N)
      {
        var r = expected_floating_rate(i) / frequency;
        denominator += 1.0 / math.pow( 1 + r/100, i + 1);
      }

      fixed_rate = fixed_rate / denominator * frequency;
      return fixed_rate;
  }
}