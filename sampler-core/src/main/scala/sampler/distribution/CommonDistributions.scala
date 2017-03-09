package sampler.distribution

import sampler.distribution.Distribution.from
import sampler.samplable.SamplableSyntax

import scala.annotation.tailrec

trait CommonDistributions
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

	def piecewiseLinear(points: (Double, Double)*): Distribution[Double] ={
		case class Segment(xPositionOffset: Double, width: Double, gradient: Double, intercept: Double) {
			assume(intercept >= 0)
			assume(width > 0)

			val m = math.abs(gradient)
      val mSign = if(m > 0) 1 else -1
			val c = intercept
			val integral = (m * width * 0.5 + c) * width

			def inverse(y: Double) = math.sqrt(y * mSign + c * c / (2 * m)) / math.sqrt(m / 2) - c / m

      val dist = Distribution.uniform(0.0, 1.0).map(r => inverse(r * integral) + xPositionOffset)
		}

    val segments = points.sliding(2).map{ case Seq((x1, y1), (x2, y2)) =>
        assume(x1 < x2)
        val width = x2 - x1
        val gradient = (y2 - y1) / width
        val intercept = y1
        Segment(x1, width, gradient, intercept)
      }
        .toIndexedSeq

    val segmentDist = Distribution.fromWeightsTable(
      segments.map(s => s -> s.integral).toMap
    )

    segmentDist.flatMap(_.dist)   //Choose the segment, then choose value in segment
	}
}

object Test extends App {
  val d = Distribution.piecewiseLinear((1,100), (4,2), (5,10), (6,1))
  (1 to 5).map(_ => d.sample(sampler.maths.Random)).foreach(println)
}