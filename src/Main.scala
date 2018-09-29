import Option._;
import Option.Binomial._;

object Main {
  def main(args: Array[String]): Unit = {
    val X = 100; 
    val S = 95;
    val maturity = 0.5;
    val volatility = 0.3;
    val riskFree = 0.08;
        
    for(step <- 1 to 1000){
      var tree: BinomialTree = new BinomialTree(X, S, maturity, volatility, riskFree, step, OptionType.Call);
      var value = tree.calcOptionValue();
      println(value);
    }
  }
}