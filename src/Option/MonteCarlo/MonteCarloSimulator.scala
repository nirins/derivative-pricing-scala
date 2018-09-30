package Option.MonteCarlo

import scala.collection._
import Option.OptionType._
import Option.MonteCarlo.Discretization._

class MonteCarloSimulator (
  var assetPrice: Double,
  var strike: Double,
  var maturity: Double,
  var volatility: Double,
  var riskFree: Double,
  var steps: Int,
  var optionType: OptionType,
  var nSims: Int,
  var scheme: IDiscretizationScheme){
    
  var priceSimArray = new Array[SimulatedPrice](nSims);
  
  def runSimulation() = {    
    for(i <- 0 until nSims){
      var simPrice = new SimulatedPrice(assetPrice, maturity, volatility, riskFree, steps, scheme);
      priceSimArray(i) = simPrice;
    }

    for(i <- 0 until nSims){
      priceSimArray(i).simulatePrice();
    }        
  }
  
  def calcOptionValue(): Double = {
    var value = 0.0;
    
    optionType match {
      case Call => 
        value = priceSimArray.map(a => a.simulatedPriceArray.last - strike).filter(b => b > 0).sum / nSims;
      case Put => 
        value = priceSimArray.map(a => strike - a.simulatedPriceArray.last).filter(b => b > 0).sum / nSims;
    }
    val discount = getDiscountFactor(riskFree, steps, maturity);    
    return value * discount;
  }
  
  def getDiscountFactor(riskFree: Double, steps: Int, maturity: Double): Double = {    
    return math.exp(-riskFree * steps * maturity / Const.kDaysInAYear);
  }  
}