//package sampler.examples
//
//import sampler.abc.ABCModel
//import sampler.math.Random
//import sampler.abc.ABCMeta
//import sampler.math.Probability
//import sampler.data.Samplable
//import sampler.abc.Prior
//import sampler.abc.ABCMethod
//import sampler.run.SerialRunner
//import java.nio.file.{Paths, Files}
//import sampler.data.Types.Column
//import sampler.io.CSVTableWriter
//import sampler.r.ScriptRunner
//import org.apache.commons.math3.distribution.BetaDistribution
//
//object Recapture extends App{
//	val wd = Paths.get("examples").resolve("Recapture")
//	Files.createDirectories(wd)
//
//	/*
//	 * Population size
//	 */
//	val encapPopulation0 = ABCMethod.init(RecaptureModel)
//	val finalEncapPopulation = ABCMethod.run(encapPopulation0, new SerialRunner).get//.population
//	val finalPopulation = finalEncapPopulation.population.map(_.value.asInstanceOf[RecaptureModel.Parameters].populationSize)
//	
//	new CSVTableWriter(wd.resolve("recapture.csv"), overwrite = true).apply(
//		Column(finalPopulation, "popSize")
//	)
//	
//	/*
//	 * Suppose our tagging and recapture yield
//	 * Total sample size of 235
//	 * Total num positive of 86
//	 * 
//	 * 
//	 * Given the known test performance what are the confidence limits
//	 * for the estimate prevalence of 86/235 = 0.36595745
//	 */
//	//Test performance
//	// High performance:	95% > 0.9, mode = 0.98
//	// new BetaDistribution(42, 1.8) 
//	// 
//	// Med performance: 	95% > 0.8, mode = 0.95 
//	// new BetaDistribution(21, 2) 
//	val se = new BetaDistribution(21, 2) 
//	val sp = new BetaDistribution(42, 1.8)
//	//val 
//	
//	
//	val rScript = 
//s"""
//lapply(c("ggplot2", "reshape", "deSolve"), require, character.only=T)
//
//recapture = read.csv("recapture.csv")
//
//pdf("plots.pdf", width=4.13, height=2.91) #A7 landscape paper
//ggplot(recapture, aes(x=popSize)) + geom_histogram(binwidth=10)
//dev.off()
//"""
//
//	ScriptRunner.apply(rScript, wd.resolve("script.r"))
//}
//
//object RecaptureModel extends ABCModel[Random] with Serializable{
//	//We initially capture and tag 50, then later sample 220, of which 35 are tagged
//	val numberTagged = 50
//	val observations = Observations(220, 35)
//
//	val random = new Random()
//    val meta = new ABCMeta(
//    	reps = 10,
//		numParticles = 100, 
//		refinements = 10,
//		particleRetries = 10, 
//		particleChunking = 100
//	)
//	
//    case class Parameters(populationSize: Int) extends ParametersBase with Serializable{
//      val threeDie = Samplable.uniform(IndexedSeq(-1,0,0,1))
//      private def threeDensity(v: Int) = v match{
//      	case -1 => 0.25
//      	case 1 => 0.25
//      	case 0 => 0.5
//      	case _ => 0
//      }
//		
//      def perturb(random: Random) = Parameters(populationSize + threeDie.sample(random))
//      def perturbDensity(that: Parameters) = threeDensity(populationSize - that.populationSize)
//    }
//
//    case class Observations(numSampled: Int, numRecaptured: Int) extends ObservationsBase with Serializable
//    
//    case class Output(obs: Observations) extends OutputBase with Serializable{
//      def distanceTo(otherObs: Observations): Double = {
//    	assert(obs.numSampled == otherObs.numSampled)
//      	val dist = math.abs(obs.numRecaptured - otherObs.numRecaptured)
////      	println("  "+obs+" , "+otherObs)
//      	dist
//      }
//    }
//    
//    def samplableModel(p: Parameters, obs: Observations) = {
////    	println("model with pop size "+p.populationSize)
////    	val numTagged = 10
//    	
//		def numTaggedDistribution(numTagged: Int, populationSize: Int, sampleSize: Int): Samplable[Output, Random] = {
//    	  val population = (1 to populationSize).map(_ <= numTagged)
//    	  val model = Samplable.withoutReplacement(population, sampleSize)
//				.map(_.count(identity))
//				.map(numRecaptured => {
//					//println(numRecaptured)
//					Output(Observations(sampleSize, numRecaptured))
//				})
//		  model
//		}
//    	
//    	numTaggedDistribution(numberTagged, p.populationSize, obs.numSampled)
//    }
//    
//    val prior = new Prior[Parameters, Random] with Serializable{
//    	val upperLimit = 1000
//    	val lowerLimit = 250
//    	def density(p: Parameters) = {
//	      if(p.populationSize > upperLimit || p.populationSize < lowerLimit) 0.0
//	      else 1.0 / (upperLimit - lowerLimit)
//	    }
//	    
//	    def sample(implicit random: Random) = Parameters(random.nextInt(upperLimit) + 1)
//    }
//}