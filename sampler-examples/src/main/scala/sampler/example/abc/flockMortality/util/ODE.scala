package sampler.example.abc.flockMortality.util

import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import scala.collection.mutable.Buffer
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator
import org.apache.commons.math3.ode.sampling.FixedStepHandler
import org.apache.commons.math3.ode.sampling.StepNormalizer
import scala.Array.canBuildFrom

case class ODEState(s: Double, e: Double, i: Double, r: Double, d: Double, eggs: Double)
object ODEState {
  
  // Converts list of integers to list of their cumulative sum
  def toCumulative(daily: IndexedSeq[Double]): IndexedSeq[Double] = {
    daily.scanLeft(0.0){case (a, v) => a + v}.tail
  }
  
  def roundValue(value: Double) = if (value < 1) 0.0 else value
  
  def rounded(states: List[ODEState]) = {
    
    val numDays = states.length
    val dailyDead = (0 until numDays).map{ day =>
      if (day == 0)
        states(day).d
      else
        states(day).d - states(day-1).d
    }       
    
    val cumulativeDead = toCumulative(dailyDead.map( x => roundValue(x) ))
    
    (0 until numDays).map{ day =>
      ODEState(states(day).s, states(day).e, states(day).i, states(day).r, cumulativeDead(day), states(day).eggs)
    }
  }
  
  def roundDeadAndEggs(states: List[ODEState]) = {
    
    val numDays = states.length
    val dailyDead = (0 until numDays).map{ day =>
      if (day == 0)
        states(day).d
      else
        states(day).d - states(day-1).d
    }
    
    val cumulativeDead = toCumulative(dailyDead.map( x => if (x < 1) roundValue(x) else x.round ))
    
    (0 until numDays).map{ day =>
      ODEState(states(day).s, states(day).e, states(day).i, states(day).r, cumulativeDead(day), states(day).eggs.round)
    }
  }
  
}

case class ODE(p: Parameters, mu: Double = 0.0) extends FirstOrderDifferentialEquations {        
    def getDimension() = 5
    def computeDerivatives(time: Double, y: Array[Double], yDot: Array[Double]) {
      import p._
      
      /*
     * beta = transmission rate
     * eta = 1 / latent period
     * gamma = 1 / infectious period
     * delta = mortality rate
     * sigma = rate of egg production for infectious birds
     * sigma2 = rate of egg production for recovered birds
     * offset = start day of infection
     * mu = baseline mortality rate (can be turned off if mu = 0)
     */
      
      // No baseline mortality (or can set mu = 0)
//      yDot(0) = - beta * y(0) * y(2)                // S: -beta S I
//      yDot(1) = beta * y(0) * y(2) - eta * y(1)     // E: beta S I - eta E
//      yDot(2) = eta * y(1) - gamma * y(2)           // I: eta E - gamma I
//      yDot(3) = (1 - delta) * gamma * y(2)          // R: (1-delta) gamma I
//      yDot(4) = delta * gamma * y(2)                // D: delta gamma I
      
      // Include baseline mortality at a rate mu
      yDot(0) = - beta * y(0) * y(2) - mu * y(0)         // S: -beta S I - mu S
      yDot(1) = beta * y(0) * y(2) - eta * y(1)     // E: beta S I - eta E
      yDot(2) = eta * y(1) - gamma * y(2)           // I: eta E - gamma I
      yDot(3) = (1 - delta) * gamma * y(2) - mu * y(3)          // R: (1-delta) gamma I - mu R
      yDot(4) = delta * gamma * y(2) + mu * (y(0)+y(3))        // D: delta gamma I + mu (S+R)
      
    }
}
object ODE {
  
  def run(y0: Array[Double], numDays: Int, p: Parameters, eggCoeff: Double, baselineMortalty: Double = 0.0): List[ODEState] = {
    
    val timeZero = 0.0
    val relTol = 1e-11; val absTol = 1e-11
    val minStep = 1e-8; val maxStep = 100.0
    
    val steps = Buffer[ODEState]()
    
    val stepHandler = new FixedStepHandler() {
      def init(t0: Double, y0: Array[Double], t: Double) {}
      def handleStep(t: Double, y: Array[Double], yDot: Array[Double], isLast: Boolean) {
        val rounded = y.map{value => if(math.abs(value) < 1e-4) 0 else value }
        val state = ODEState(
          rounded(0), 
          rounded(1), 
          rounded(2), 
          rounded(3), 
          rounded(4), 
          eggCoeff*(rounded(0)+rounded(1)) + p.sigma*rounded(2) + p.sigma2*rounded(3)
        )
        steps.append(state)
      }
    }
    
//    }
    
    val stepNormaliser = new StepNormalizer(1.0, stepHandler)        
    val dp853 = new DormandPrince853Integrator(minStep, maxStep, absTol, relTol)
    val out = Array[Double](0,0,0,0,0) // Not really used, since the step handler gets the output
    dp853.addStepHandler(stepNormaliser)
    
    dp853.integrate(new ODE(p, baselineMortalty: Double), 0.0, y0, numDays, out)
    
    steps.toList
  }
  
  
}