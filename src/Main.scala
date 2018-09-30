import Option._
import Option.OptionType._
import Option.Binomial._
import Option.MonteCarlo._
import Option.MonteCarlo.Discretization._
import Option.BlackScholes._

object Main {
  def main(args: Array[String]): Unit = {
    val X = 100; 
    val S = 95;
    val maturity = 0.5;
    val volatility = 0.3;
    val riskFree = 0.08;
    val optionType = OptionType.Call;
    
    runBinomial(X, S, maturity, volatility, riskFree, optionType);
    runMonteCarlo(X, S, maturity, volatility, riskFree, optionType);
    runBlackScholes(X, S, maturity, volatility, riskFree, optionType);
  }
  
  def runBinomial(X: Double, S:Double, maturity: Double, volatility: Double, riskFree: Double, optionType: OptionType) = {
    for(step <- 1 to 1000){
      var tree: BinomialTree = new BinomialTree(X, S, maturity, volatility, riskFree, step, optionType);
      val value = tree.calcOptionValue();
      println("Binomial", value);
    }    
  }
  
  def runMonteCarlo(X: Double, S:Double, maturity: Double, volatility: Double, riskFree: Double, optionType: OptionType) = {    
    val steps = 252;
    val nSims = 100000;
    val scheme = new EulerDiscretization();
    var simulator = new MonteCarloSimulator(X, S, maturity, volatility, riskFree, steps, optionType, nSims, scheme);
    simulator.runSimulation();
    val value = simulator.calcOptionValue();
    println("Monte Carlo", value);
  }
  
  def runBlackScholes(X: Double, S:Double, maturity: Double, volatility: Double, riskFree: Double, optionType: OptionType) = {    
    val value = BlackScholesFormula.calcOptionValue(X, S, maturity, volatility, riskFree, optionType);
    println("Black Scholes", value);
  }
  
}