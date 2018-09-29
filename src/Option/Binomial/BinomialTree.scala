package Option.Binomial
import Option.OptionType._

class BinomialTree (
    var assetPrice: Double,
    var strike: Double,
    var maturity: Double,
    var volatility: Double,
    var riskFree: Double,
    var steps: Int,
    var optionType: OptionType) {

  def calcOptionValue(): Double = {
    var total = 0.0;
    var u = Math.exp(this.volatility * Math.sqrt(this.maturity / this.steps));
    var d = 1 / u;
    var f = getFutureValue(1.0, this.riskFree, this.maturity / this.steps);
    var p = (f-d)/(u-d);
    
    for (i <- 0 to this.steps){
      var X = this.assetPrice * Math.pow(u, i) * Math.pow(d, this.steps - i);
      var payoff = getPayoff(X, this.strike, this.optionType);
      var node = getNodeProbability(i, this.steps, p);
      total += node * payoff;
    }
    
    var presentValue = getPresentValue(total, this.riskFree, this.maturity);    
    return presentValue;
  }
    
  def getBinomialCoefficient(m: Int, n: Int): Double = {
    var coef: Double = 1.0;    
    for(i <- 0 until m){
      coef *=  (n - i);
      coef /=  (m - i);
    }    
    return coef;
  }
  
  def getNodeProbability(m: Int, n: Int, p: Double): Double = {
    return getBinomialCoefficient(m, n) * Math.pow(p, m) * Math.pow(1-p, n - m);
  }
 
  def getPayoff(S: Double, X: Double, optionType: OptionType): Double = {
    optionType match {
      case Call => return Math.max(0.0, X-S);
      case Put => return Math.max(0.0, S-X);
      return 0.0;
    }
  }
  
  def getFutureValue(P: Double, r: Double, n: Double): Double = {
    return P * Math.pow(1.0 + r, n);    
  }

  def getPresentValue(F: Double, r: Double, n: Double): Double = {
    return F / Math.pow(1.0 + r, n);    
  }  
}