package sampler.abc.actor.sub.flushing

import sampler.abc.Population
import sampler.abc.actor.main.component.helper.Getters
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.actor.main.WeighedParticles
import sampler.abc.actor.main.component.helper.Getters
import sampler.abc.ABCConfig
import java.nio.file.Paths
import apha.tbmi.input.config.FittingParameters
import java.time.LocalDateTime
import org.apache.commons.io.FileUtils

class GenerationFlusher(
		toleranceCalculator: ToleranceCalculator,
		observedIdsTrimmer: ObservedIdsTrimmer,
		weightsConsolidator: WeightsHelper,
		getters: Getters,
		config: ABCConfig
	){
	
	def apply[P](gen: EvolvingGeneration[P]) = {
		val weighedParticles = gen.weighed
		val currentTolerance = gen.currentTolerance
		val currentGeneration = gen.buildingGeneration
		val idsObserved = gen.idsObserved
		
		
		val wd = Paths.get("")
		import play.api.libs.json._

		val details: Seq[Map[String, JsValue]] = weighedParticles.seq.map{wp =>
		  val fp = wp.value.scored.params.asInstanceOf[FittingParameters]
		  
		  Map(
		    "s" -> JsArray(wp.value.scored.repScores.map(d => JsNumber(d))),
		    "w" -> JsNumber(wp.value.weight),
		    "p" -> JsObject(Map(
		        "EnvDecy" -> JsNumber(fp.getEnvDecayRate()),
		        "FarmEnv" -> JsNumber(fp.getFarmEnvRate()),
		        "InfSens" -> JsNumber(fp.getInfectiousSensitivity()),
		        "PrshEnv" -> JsNumber(fp.getParishEnvRate()),
		        "S-to-T" -> JsNumber(fp.getsToTRate()),
		        "TstSens" -> JsNumber(fp.getTestSensitiveSensitivity()),
		        "T-to-I" -> JsNumber(fp.gettToIRate())
		      )
		    )
		  )
		}
		  
	  val pop = Json.obj(
		  "meta" -> Map(
				  "comment" -> "LIFEBOAT!!!!",
				  "created" -> LocalDateTime.now.toString        
		  ),
	    "generation" -> currentGeneration,
  		"acceptance-ratio" -> weighedParticles.acceptanceRatio,
  		"tolerance" -> currentTolerance,
		  "particle-details" -> details
		)
		
		val jsonPretty = Json.prettyPrint(pop)
  	FileUtils.write(wd.resolve("lifeboat.json").toFile, jsonPretty)
		
		//Strip out tags
		val seqWeighed = getters.weighedParticlesWithoutIdTags(weighedParticles)
		
		assert(config.numParticles <= seqWeighed.size)
		
		val completedGen = Population(
				weightsConsolidator.consolidateToWeightsTable(seqWeighed),
				currentGeneration, 
				currentTolerance,
				weighedParticles.acceptanceRatio
		)
			
		EvolvingGeneration(
			toleranceCalculator(seqWeighed, config, currentTolerance),
			completedGen,
			ScoredParticles.empty,
			WeighedParticles.empty,
			observedIdsTrimmer(idsObserved)
		)
	}
}