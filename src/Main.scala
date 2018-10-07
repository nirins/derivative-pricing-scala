import Option._
import Option.OptionType._
import Option.Binomial._
import Option.MonteCarlo._
import Option.MonteCarlo.Discretization._
import Option.BlackScholes._
import Swap._
import java.time._
import java.time.format._

object Main {
  def main(args: Array[String]): Unit = {
    val X = 100; 
    val S = 95;
    val maturity = 1;
    val volatility = 0.3;
    val riskFree = 0.08;
    val optionType = OptionType.Call;
    
    runBinomial(X, S, maturity, volatility, riskFree, optionType);
    runMonteCarlo(X, S, maturity, volatility, riskFree, optionType);
    runBlackScholes(X, S, maturity, volatility, riskFree, optionType);
    runVanillaSwap();
  }
  
  def runBinomial(X: Double, S:Double, maturity: Double, volatility: Double, riskFree: Double, optionType: OptionType) = {
    println("Binomial");
    for(step <- 1 to 1000){
      var tree: BinomialTree = new BinomialTree(X, S, maturity, volatility, riskFree, step, optionType);
      val value = tree.calcOptionValue();
      println(value);
    }    
  }
  
  def runMonteCarlo(X: Double, S:Double, maturity: Double, volatility: Double, riskFree: Double, optionType: OptionType) = {    
    println("Monte Carlo");
    val steps = 252;
    val nSims = 1000;
    val scheme = new EulerDiscretization();

    for(i <- 1 to nSims){
      var simulator = new MonteCarloSimulator(X, S, maturity, volatility, riskFree, steps, optionType, i, scheme);
      simulator.runSimulation();
      val value = simulator.calcOptionValue();
      println(value);      
    }
  }
  
  def runBlackScholes(X: Double, S:Double, maturity: Double, volatility: Double, riskFree: Double, optionType: OptionType) = {    
    val value = BlackScholesFormula.calcOptionValue(X, S, maturity, volatility, riskFree, optionType);
    println("Black Scholes");
    println(value);
  }
  
  def runVanillaSwap() = {
    println("Vanilla Swap");
    
    val from_date = LocalDate.of(1986, 1, 1);
    val to_date = LocalDate.of(2013, 5, 31);
    
    val vs = new VanillaSwap();
    vs.contructLIBORCurve();
    val swap_curve = vs.generateSwapCurve(from_date, to_date);
    
    for(s <- swap_curve ){
      println(s);
    }    
  }
  
}