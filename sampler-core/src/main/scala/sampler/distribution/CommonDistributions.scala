package sampler.distribution

import sampler.samplable.SamplableSyntax
import sampler.distribution.Distribution.from
import scala.annotation.tailrec
import sampler.maths.Partition
import sampler.maths.AliasTable

object CommonDistributions 
    extends DistributionImplicits
    with SamplableSyntax {
  
  def always[T](value: T): Distribution[T] = Pure(value)
  
  /** Uniform distribution of doubles
	 *  
	 *  @param lower the lower bound of the distribution (inclusive)
	 *  @param upper the upper bound of the distribution (exclusive) */
	def uniform(lower: Double, upper: Double) = from{r =>
		(upper - lower) * r.nextDouble() + lower
	}
  
 /** Uniform distribution of integers
	 *  
	 *  @param lower the lower bound of the distribution (inclusive)
	 *  @param upper the upper bound of the distribution (exclusive) */
	def uniform(lower: Int, upper: Int) = from{r =>
		r.nextInt(upper - lower) + lower
	}
  
  /** Samples from a an indexed sequence of values with uniform weighting.
	 *  
	 *  @param items the sequence of values to be sampled from */
	def uniform[T](items: IndexedSeq[T]) = {
		val size = items.size
	  from{r => items(r.nextInt(size))}
	}
	
	/** Builds a new [[sampler.data.Distribution]] which allows a set of items to be sampled according 
	 *  to a normalised weighting.
	 *  
	 *  @param items the items which are to be sampled
	 *  @param p [[sampler.math.Partition]] containing the probabilities of sampling each object
	 */ 
	//TODO isn't this just the same as Distribution.fromTable?
	def fromWeightsTable[T](wTable: Map[T, Double]): Distribution[T] =
		wTable.toDistribution
	
	/** Samples from multiple values without replacement using uniform weighting.
	 *  
	 *  @param items the sequence of values to be sampled from 
	 *  @param sampleSize the number of items to be selected from the set
	 *  
	 *  @example
	 *  {{{
	 *  implicit val r = Random
	 *  val model = Distribution.withoutReplacement(IndexedSeq("red", "blue", "green", "yellow"), 2)
	 *  
	 *  model.sample
	 *  res1: List[String] = List(blue, green)
	 *  }}}
	 *  */
	//TODO reverse list?
	def withoutReplacement[T](items: IndexedSeq[T], sampleSize: Int) = from{r => 
		def sample() = {
			@tailrec
			def takeAnother(acc: List[T], bag: IndexedSeq[T]): List[T] = {
				if(acc.size == sampleSize) acc
				else{ 
					val item = bag(r.nextInt(bag.size))
					takeAnother(item +: acc, bag diff List(item))
				}
			}
				
			takeAnother(Nil, items)
		}
	}
	
	//TODO test, urgently!
	//TODO reverse list?
	def withoutReplacement(
			populationSize: Int, 
			populationTrue: Int, 
			stopWhen: IndexedSeq[Boolean] => Boolean = _ => false ) = {
		assert(populationSize >= populationTrue)
		from{r => 
			@tailrec def take(acc: IndexedSeq[Boolean], nTrue: Int, size: Int): IndexedSeq[Boolean] = {
				if(size ==0 || stopWhen(acc)) acc
				else {
					val item = r.nextInt(size) <= nTrue
					take(acc :+ item, if(item) nTrue - 1 else nTrue, size - 1)
				}
			}
			take(IndexedSeq.empty[Boolean], populationSize, populationTrue)
		}
	}
	
	/** Coins toss
	 *  
	 *  @param probSuccess the probability of success */
	def bernoulli(probSuccess: Double): Distribution[Boolean] = from{r =>
	  r.nextBoolean(probSuccess)
	}
	
  //TODO test
	def binomial(probSuccess: Double, trials: Double) = 
		bernoulli(probSuccess)
			.until(_.size == trials)
			.map(_.count(identity))
			
	//TODO test
	def negativeBinomial(numFailures: Int, probSuccess: Double) = {
		bernoulli(probSuccess)
			.until(_.count(!_) == numFailures)
			.map(_.size)
	}
	
  //TODO test
	def geometric(probSuccess: Double) = 
		bernoulli(probSuccess)
			.until(_.last)
			.map(_.size)
			
  //TODO test
	def hypergeometric(trials: Int, populationSize: Int, populationSuccesses: Int) =
		withoutReplacement(populationSize, populationSuccesses,_.size == trials)
			.map(_.count(identity))
			
	//TODO test
	def exponential(rate: Double) = 
		uniform(0,1).map(x => - math.log(x) / rate)
		
	//TODO test
	def poisson(rate: Double) = 
		exponential(rate).until(_.sum >= 1).map(_.size - 1)
}