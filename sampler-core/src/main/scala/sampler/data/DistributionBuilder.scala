package sampler.data

import sampler.math.Random
import scala.annotation.tailrec
import sampler.math.Partition
import sampler.math.AliasTable

object DistributionBuilder extends DistributionBuilder

trait DistributionBuilder {
	/** Builds a [[sampler.data.Distribution]] which delegates sampling to the 
	 *  call-by-name arg.
	 * 
	 * @param function supplying the sampled value */
	def apply[T](f: => T) = new Distribution[T]{
		def sample() = f
	}
	
	/** Builds a [[sampler.data.Distribution]] which always samples the same value.
	 *  
	 *  @param value the value to be returned when sampled */
	def continually[T](value: T) = new Distribution[T]{
		def sample() = value
	}
	
	/** Builds a [[sampler.data.Distribution]] which represents a Uniform distribution, 
	 *  of doubles
	 *  
	 *  @param lower the lower bound of the distribution (inclusive)
	 *  @param upper the upper bound of the distribution (exclusive) */
	def uniform(lower: Double, upper: Double)(implicit r: Random) = new Distribution[Double]{
		def sample() = (upper - lower) * r.nextDouble() + lower
	}
	
	/** Builds a [[sampler.data.Distribution]] which represents a Uniform distribution 
	 *  of integers between the lower (inclusive) and upper (exclusive) values
	 *  
	 *  @param lower the lower bound of the distribution (inclusive)
	 *  @param upper the upper bound of the distribution (exclusive) */
	def uniform(lower: Int, upper: Int)(implicit r: Random) = new Distribution[Int]{
		def sample() = r.nextInt(upper - lower) + lower
	}
	
	/** Builds a [[sampler.data.Distribution]] which allows sampling from a an indexed
	 *  sequence of values with uniform weighting.
	 *  
	 *  @param items the sequence of values to be sampled from */
	def uniform[T](items: IndexedSeq[T])(implicit r: Random) = new Distribution[T]{
		val size = items.size
		def sample() = items(r.nextInt(size))
	}
	
	/** Builds a [[sampler.data.Distribution]] which samples multiple 
	 *  values without replacement with uniform weighting.
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
	def withoutReplacement[T](items: IndexedSeq[T], sampleSize: Int)(implicit r: Random) = new Distribution[List[T]]{
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
			stopWhen: IndexedSeq[Boolean] => Boolean = _ => false
	)(implicit r: Random) = new Distribution[IndexedSeq[Boolean]]{
		assert(populationSize >= populationTrue)
		def sample() = {
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

	/** Builds a [[sampler.data.Distribution]] to flip coins.
	 *  
	 *  @param probSuccess the probability of success when sampling from this object */
	def bernoulli(probSuccess: Double)(implicit r: Random) = new Distribution[Boolean]{
	  def sample() = r.nextBoolean(probSuccess)
	}
	
	//TODO test
	def binomial(probSuccess: Double, trials: Double)(implicit r: Random) = 
		bernoulli(probSuccess)(r)
			.until(_.size == trials)
			.map(_.count(identity))
			
	//TODO test
	def negativeBinomial(numFailures: Int, probSuccess: Double)(implicit r: Random) = {
		bernoulli(probSuccess)(r)
			.until(_.count(!_) == numFailures)
			.map(_.size)
	}
	
	//TODO test
	def geometric(probSuccess: Double)(implicit r: Random) = 
		bernoulli(probSuccess)(r)
			.until(_.last)
			.map(_.size)
	
	//TODO test
	def hypergeometric(trials: Int, populationSize: Int, populationSuccesses: Int)(implicit r: Random) =
		withoutReplacement(populationSize, populationSuccesses,_.size == trials)
			.map(_.count(identity))
		
	//TODO test
	def exponential(rate: Double)(implicit r: Random) = 
		uniform(0,1).map(x => - math.log(x) / rate)
			
	//TODO test
	def poisson(rate: Double)(implicit r: Random) = 
		exponential(rate).until(_.sum >= 1).map(_.size - 1)
	
	
	/** Builds a [[sampler.data.Distribution]] to sample from an indexed sequence of values
	 *  according to the probabilities in a [[sampler.math.Partition]].  Uses the alias method: 
	 *  [[sampler.math.AliasTable]]
	 *  
	 *  @param items the items to be sampled from
	 *  @param p [[sampler.math.Partition]] containing the probabilities of sampling each object
	 */ 
	def fromPartition[T](items: IndexedSeq[T], p: Partition)(implicit r: Random) = new Distribution[T]{
	  private def theSame(a: IndexedSeq[T], b: Partition) = items.size == p.size
	  
	  assert(theSame(items, p), s"Expected both objects to have the same length")
	  
	  val aliasTable = new AliasTable(p)
	  def sample() = items(aliasTable.next(r))
	}
	
	/** Builds a new [[sampler.data.Distribution]] which allows a set of items to be sampled according 
	 *  to a normalised weighting.
	 *  
	 *  @param items the items which are to be sampled
	 *  @param p [[sampler.math.Partition]] containing the probabilities of sampling each object
	 */ 
	def fromWeightsTable[T](wTable: Map[T, Double])(implicit r: Random): Distribution[T] = {
		val (parameterSets, weights) = wTable.toIndexedSeq.unzip
		fromPartition(parameterSets, Partition.fromWeights(weights))
	}
}