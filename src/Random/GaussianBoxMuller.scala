package Random
import math._;

object GaussianBoxMuller {
  def random(): Double = {
    var x: Double = 0.0;
    var y: Double = 0.0;
    var square: Double = 1.0;
    
    while(square >= 1){
     x = 2 * math.random() - 1; 
     y = 2 * math.random() - 1; 
     square = (x * x) + (y * y);
    }
   
    return x * math.sqrt(-2 * math.log(square) / square);
  }
}