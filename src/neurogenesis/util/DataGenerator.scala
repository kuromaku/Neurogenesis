package neurogenesis.util
/*This class is only used for generating simple data for testing purposes
 * 
 */
class DataGenerator {
  def createInputData2(num:Int) : Array[Array[Double]] = {
    val r = Array.ofDim[Double](num,4)
    for (i <- 0 until num) {
      r(i)(0) = (0.5)*scala.math.sin(1.0*i/(num/7))+scala.math.random*0.1-0.05
      r(i)(1) = (0.5)*scala.math.sin(1.0*i/(num/11))+scala.math.random*0.1-0.05
      r(i)(2) = (0.5)*scala.math.cos(1.0*i/(num/17))+scala.math.random*0.1-0.05
      r(i)(3) = .5*scala.math.cos(1.0*i*14/num)
    }
    r
  }
  def createOutputData2(in:Array[Array[Double]]) : Array[Array[Double]] = {
    var r = Array.ofDim[Double](in.length,2)
    val num = in.length
    for (i <- 0 until in.length) {
      r(i)(0) = in(i)(1)/2+in(i)(2)/3
      r(i)(1) = in(i)(0)-in(i)(2)
    }
    for (i <- 3 until in.length) {
      if (in(i)(0) > 0) {
        r(i)(0) += (0.5)*scala.math.sin(1.0*(i-3)/(num/7))+in(i)(3)
      }
      else {
        r(i)(0) += (0.5)*scala.math.sin(1.0*(i-3)/(num/7))*0.3+in(i)(3)
      }
      r(i)(1) -= (0.5)*scala.math.sin(1.0*(i-1)/(num/11))-(0.5)*scala.math.cos(1.0*(i-2)/(num/17))
    }
    
    r
  }
  def sinsum(x:Double) : Double = {
    val y = math.sin(0.2*x)+math.sin(0.311*x)+math.sin(0.42*x)+math.sin(0.74*x)
    y
  }
  def sinsum2(x:Double) : Double = {
    val y = math.sin(0.12*x)+math.sin(0.271*x)+math.sin(0.571*x)+math.sin(0.891*x)
    y   
  }
  def createInputData(num:Int) : Array[Array[Double]] = {
    val r = Array.ofDim[Double](num,2)
    for (i <- 0 until num) {
      r(i)(0) = sinsum(100*i*math.Pi/num)
      r(i)(1) = sinsum2(100*i*math.Pi/num)
    }
    r
  }
  def createOutputData(in:Array[Array[Double]]) : Array[Array[Double]] = {
    var r = Array.ofDim[Double](in.length,2)
    val num = in.length
    for (i <- 0 until in.length-1) {
      r(i)(0) = in(i+1)(0)
      r(i)(1) = in(i+1)(1)
    }
    r(in.length-1)(0) = r(in.length-2)(0)
    r(in.length-1)(1) = r(in.length-2)(1)
    r
  }
  def createDataset(num:Int) : (Array[Array[Double]],Array[Array[Double]]) = {
    val in = createInputData(num+10)
    val out = createOutputData(in.drop(10))
    
    (in.dropRight(10),out)
  }
  /*
   * P
  by n math.sin(λi x), where n is the number of math.sine waves and
  i=1
  λ1 = 0.2, λ2 = 0.311, λ3 = 0.42, λ4 = 0.51, and λ5 = 0.74

   */
}