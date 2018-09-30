package Option.MonteCarlo.Discretization
import math._;
import Random._;

class EulerDiscretization extends IDiscretizationScheme {
  
  override def increment(value: Double, maturity: Double, volatility: Double, riskFree: Double): Double = {    
    return value * (1 
        + riskFree * maturity / Const.kDaysInAYear
        + volatility * math.sqrt(maturity / Const.kDaysInAYear) * GaussianBoxMuller.random());
  }  
  
}