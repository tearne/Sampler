package sampler.spike.farrington

//import org.apache.commons.math3.distribution.PoissonDistribution


/*
  =========
  NOTES:
  Function simulates outbreak data for the EDS using a negative binomial
  model with dispersion parameter >= 1
  (Note that a Poission distribution is used if dispersion parameter == 1)
  
  Follows the method outlined in
  Noufaily et al., Statist. Med. 2013 (32) 1206-1222
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      19/02/2015
  Last edit: 19/02/2015
  
  ==========
  INPUTS:

	nYears       No. of years for which to simulate data
	period       Intervals at which to simulate data ("weekly" or "monthly")
	
	alpha        
	beta         Linear trend
	m            Seasonality (0 = none, 1 = annual, 2 = biannual)
	gamma_1      Controls magnitude of peaks
	gamma_2      Controls magnitude of troughs
	dispersion   Dispersion parameter
	
	=========
	OUTPUTS:
		
	sim_data   Simulated outbreak data
 
 */

object simulateData  extends App {
  

  //=======================
  // User-defined parameters
  
  // Number of years for which to simulate data:
  val nYears = 10
  
  // Choose to simulate data "weekly" or "monthly"
  // period = "weekly"
  val period = "monthly"
  
  // Set of parameters to control trend, seasonality, etc
  val alpha = 1.5
  val beta = 0
  val m = 1
  val gamma_1 = 0.2
  val gamma_2 = -0.4
  val dispersion = 1.0
  
  
  //=======================
  // Simulation
  
  // Calculate number of intervals in a year (e.g. 12 if looking monthly)
  val nIntervals: Int = if (period == "weekly") 52 else 12
  
  val nData = nYears*nIntervals
  
  // Function which performs sum of a function from integer a to integer b
  def sumFunction(f: Int => Double)(a: Int, b: Int): Double = {
    def loop(a: Int, acc: Double): Double = {
      if (a > b) acc
      else loop(a+1, acc + f(a))
    }
    loop(a,0)
    
  }
  
  // Define function of j which appears in the sum from j= 1 to m:
  def sumTerm(j: Int): Double = gamma_1*math.cos(2*math.Pi*j*nYears) + gamma_2*math.sin(2*math.Pi*j*nYears)
  
  // Calculate mean counts
  val mean_count = math.exp(alpha + beta*nData + sumFunction(sumTerm)(1,m))
  
  //val sim_data = new PoissonDistribution(mean_count, nData)
    
  //val n = mean_count / (dispersion - 1)
  //val prob = 1 - (1/dispersion)
  
}