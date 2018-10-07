package Swap

import scala.collection._
import java.util.Date
import java.text._
import java.time._
import java.time.format._
import util.control.Breaks._

import Entity._

class VanillaSwap {
  var libor3m = Seq[(LocalDate, Double)]();
  var libor6m = Seq[(LocalDate, Double)]();
  var libor9m = Seq[(LocalDate, Double)]();
  var libor12m = Seq[(LocalDate, Double)]();
  
  def contructLIBORCurve() = {
    libor3m = LIBORRate.getLIBOR3M();
    libor6m = LIBORRate.getLIBOR6M();
    libor9m = LIBORRate.getLIBOR9M();
    libor12m = LIBORRate.getLIBOR12M();
  }
   
  def generateSwapCurve(from_date: LocalDate, to_date: LocalDate): Seq[(LocalDate, Double)] = {        
    var current_date = from_date;    
    var swap_curve = new scala.collection.mutable.MutableList[(LocalDate, Double)]();
        
    while(current_date.compareTo(to_date) < 0){
      breakable{
        val l3m = libor3m.filter(t => t._1.compareTo(current_date) >= 0).head._2;
        val l6m = libor6m.filter(t => t._1.compareTo(current_date) >= 0).head._2;
        val l9m = libor9m.filter(t => t._1.compareTo(current_date) >= 0).head._2;
        val l12m = libor12m.filter(t => t._1.compareTo(current_date) >= 0).head._2;
        
        if(l3m==0)
          break;
        
        var floating_rate = List(l3m, l6m, l9m, l12m);
        var p = calcFairFixRate(floating_rate);
        swap_curve += current_date -> p;  
      }
      current_date = current_date.plusDays(1);
    }
    
    return swap_curve.toList.toSeq;
  }
  
  def calcFairFixRate(floating_rate: Seq[Double]): Double = {
      var frequency = 4;
      var N = 12;

      var sum_discount_factor = 0.0;
      for (i <- 0 until frequency)
      {
        val discount_factor = 1.0 / ( math.pow(1 + floating_rate(i) / N, (i + 1) * ( N / frequency)));
        sum_discount_factor += discount_factor;
      }

      val fixed_rate = (100 - 100 / ( 1 + floating_rate.last ) ) / sum_discount_factor * frequency;
      return fixed_rate;
  }
}