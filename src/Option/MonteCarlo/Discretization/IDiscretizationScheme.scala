package Option.MonteCarlo.Discretization

trait IDiscretizationScheme {
  def increment(value: Double, maturity: Double, volatility: Double, riskFree: Double): Double;
}