package Option.BlackScholes

import Option.OptionType._
import math._

object BlackScholesFormula {
  
  //formula from https://en.wikipedia.org/wiki/Normal_distribution#Cumulative_distribution_function 
  def calcOptionValue(assetPrice: Double, strike: Double, maturity: Double, volatility: Double, riskFree: Double, optionType: OptionType): Double = {
    
    val sigma2 = volatility * volatility;
    val N = 1.0 / math.sqrt(2.0 * math.Pi * sigma2);
    val d1 = (math.log(strike / assetPrice) + (riskFree + 0.5 * sigma2) ) / (volatility * math.sqrt(maturity));
    val d2 = d1 - volatility * math.sqrt(maturity);
    
    var value = 0.0;
    
    optionType match {
      case Call => value = assetPrice * CDF(d1) - strike * math.exp(-riskFree * maturity) * CDF(d2);
      case Put => value = assetPrice * math.exp(-riskFree * maturity) * CDF(-d2) - strike * CDF(-d1);
    }
    
    return value;
  }
  
  def CDF(x: Double) : Double = {
    var sum = x;
    var value = x;
    
    for(i <- 1 to 100){
      value *= x * x / (2.0 * i + 1.0);
      sum += value;
    }
    
    return 0.5 + (sum / math.sqrt(2.0 * math.Pi)) * math.exp(-(x * x) / 2.0);
  }
  
}