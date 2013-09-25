package sampler.spike

import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator
import org.apache.commons.math3.ode.sampling._

object DormandPrinceODE extends App{
	case class Parameters(
			beta: Double, 
			eta: Double, 
			gamma: Double, 
			delta: Double, 
			sigma: Double, 
			sigma2: Double, 
			offset: Int
	)
	
	class SEIRD(p: Parameters) extends FirstOrderDifferentialEquations {
		import p._
		
	    def getDimension() = 5
	
	    def computeDerivatives(time: Double, y: Array[Double], yDot: Array[Double]) {
	    	yDot(0) = -beta * y(0) * y(2) 						//S: -beta S I
			yDot(1) = beta * y(0) * y(2) - eta * y(1)			//E: beta S I - gamma E
			yDot(2) = eta * y(1) - gamma * y(2) - delta * y(2)	//I: eta I - gamma I - delta I
			yDot(3) = gamma * y(2)								//R: gamma I
			yDot(4) = delta * y(2)								//D: the observed dead data
	    }
	}
	
	val stepHandler = new FixedStepHandler() {
	    def init(t0: Double, y0: Array[Double], t: Double) {}
	            
	    def handleStep(t: Double, y: Array[Double], yDot: Array[Double], isLast: Boolean) {
	        System.out.println(f"t $t%.2f, States = ${y(0)}%2.2f, ${y(1)}%2.2f, ${y(2)}%2.2f, ${y(3)}%2.2f, ${y(4)}%2.2f")
	    }
	}
	
	val stepNormaliser = new StepNormalizer(1.0, stepHandler)
	
	val parameters = Parameters(0.5,0.4,0.3,0.2,0.1,0.6,-3)
	val dp853 = new DormandPrince853Integrator(1.0e-8, 100.0, 1.0e-10, 1.0e-10)
	
	val ode = new SEIRD(parameters)
	val init = Array[Double](1999, 1, 0, 0, 0 )
	var out = Array[Double](0,0,0,0,0)

	dp853.addStepHandler(stepNormaliser)
	dp853.integrate(ode, 0.0, init, 20, out)
//	out.foreach(println)
}