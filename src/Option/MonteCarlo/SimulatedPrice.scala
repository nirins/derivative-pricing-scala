package Option.MonteCarlo

import Option.MonteCarlo.Discretization._
import scala.collection._

class SimulatedPrice (
  var assetPrice: Double,
  var maturity: Double,
  var volatility: Double,
  var riskFree: Double,
  var steps: Int,
  var scheme: IDiscretizationScheme) {
  
  var simulatedPriceArray = new Array[Double](steps);
  simulatedPriceArray(0) = assetPrice;
  
  def simulatePrice() = {
    for( i <- 1 until simulatedPriceArray.length){
     simulatedPriceArray(i) = scheme.increment(simulatedPriceArray(i-1), maturity, volatility, riskFree); 
    }
  }
}