import Option._
import Option.Binomial._
import Option.MonteCarlo._
import Option.MonteCarlo.Discretization._

object Main {
  def main(args: Array[String]): Unit = {
    val X = 100; 
    val S = 95;
    val maturity = 0.5;
    val volatility = 0.3;
    val riskFree = 0.08;
    
    runBinomial(X, S, maturity, volatility, riskFree);
    runMonteCarlo(X, S, maturity, volatility, riskFree);
  }
  
  def runBinomial(X: Double, S:Double, maturity: Double, volatility: Double, riskFree: Double) = {
    for(step <- 1 to 1000){
      var tree: BinomialTree = new BinomialTree(X, S, maturity, volatility, riskFree, step, OptionType.Call);
      val value = tree.calcOptionValue();
      println(value);
    }    
  }
  
  def runMonteCarlo(X: Double, S:Double, maturity: Double, volatility: Double, riskFree: Double) = {    
    val steps = 252;
    val nSims = 10000;
    val scheme = new EulerDiscretization();
    var simulator = new MonteCarloSimulator(X, S, maturity, volatility, riskFree, steps, OptionType.Call, nSims, scheme);
    simulator.runSimulation();
    val value = simulator.calcOptionValue();
    println(value);
  }
}