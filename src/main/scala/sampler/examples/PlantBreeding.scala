/*
* Copyright (c) Oliver Tearne (tearne at gmail dot com)
*
* This program is free software: you can redistribute it and/or modify it under the terms of
* the GNU General Public License as published by the Free Software Foundation, either version
* 3 of the License, or (at your option) any later version.
*
* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
* See the GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License along with this program.
* If not, see <http://www.gnu.org/licenses/>.
*/

package sampler.examples

import scala.annotation.tailrec
import sampler.data.Samplable
import sampler.math.Probability
import sampler.math.Random
import sampler.data.Empirical._
import sampler.math.StatisticsComponent


/*
 * Given a breeding heirarchy where donor traits are to be 
 * introduced (heterozygously) into a preferred variety, what 
 * proportion (distribution) of preferred variety is present 
 * in the final plant?
 */

case class Species(chromosomeLengths: Int*){
	val size = chromosomeLengths.sum
}

class Gene(val name: String) extends AnyVal

case class Trait(chromosome: Int, cM: Int, gene: Gene, species: Species) //Always assume heterozygous for now

case class Locus(chromosome: Int, leftTid: Boolean, cM: Int, species: Species){
	import species._
	assert(cM >= 0 && cM < chromosomeLengths(chromosome))
	val nextCM = if(chromosomeLengths(chromosome) == cM - 1) None else Some(cM + 1)
}

case class Chromatid(genes: Seq[Gene]){
	val size = genes.size
	def countOf(gene: Gene) = genes.count(_ == gene)
}
object Chromatid{
	def ofGenes(gene: Gene, length: Int) = Chromatid(Seq.fill(length)(gene))
}

case class Chromosome(left: Chromatid, right: Chromatid){
	assert(left.size == right.size)
	def countOf(gene: Gene) = left.countOf(gene) + right.countOf(gene)
}
object Chromosome {
	def ofGenes(gene: Gene, length: Int) = Chromosome(
		Chromatid.ofGenes(gene, length),
		Chromatid.ofGenes(gene, length)
	)
}

case class Plant(chromosomes: Seq[Chromosome], species: Species){ self =>
	def geneAt(locus: Locus) = {
		if(locus.leftTid) chromosomes(locus.chromosome).left.genes(locus.cM)
		else chromosomes(locus.chromosome).right.genes(locus.cM)
	}
		
	def satisfiesTrait(tr: Trait) = 
		geneAt(Locus(tr.chromosome, true, tr.cM, tr.species)) == tr.gene || geneAt(Locus(tr.chromosome, false, tr.cM, tr.species)) == tr.gene
	
	def selectFor(traits: Seq[Trait]) = {
		!traits.exists(t => !satisfiesTrait(t) || t.species != species)
	}
	
	def countOf(gene: Gene) = {
		chromosomes.foldLeft(0)(_ + _.countOf(gene))
	}
	
	implicit val r = Random
	def recombinationModel = Samplable.bernouliTrial(Probability(0.01))
	def coin = Samplable.coinToss
	
	def x(that: Plant): Plant = {
			assert(this.species == that.species)
			@tailrec
			def gameteBuilder(parent: Plant, prevLocus: Locus, acc: Seq[Gene] = Nil): Chromatid = {
				prevLocus.nextCM match{
					case None => Chromatid(acc)
					case Some(nextCM) => 
						val amOnLeftChromosome = (prevLocus.leftTid ^ recombinationModel.sample)
						val nextLocus = prevLocus.copy(leftTid = amOnLeftChromosome, cM = nextCM)
						val nextGene = parent.geneAt(nextLocus)
						gameteBuilder(parent, nextLocus, nextGene +: acc)
				}
			}
			
			val offSpringChromosomes = (0 to species.chromosomeLengths.size).map(chromosomeIdx => Chromosome(
				gameteBuilder(this, Locus(chromosomeIdx, coin.sample, 0, species)),
				gameteBuilder(that, Locus(chromosomeIdx, coin.sample, 0, species))
			))
			
			Plant(offSpringChromosomes, species)
		}
	
}
object Plant {
	def ofGenes(gene: Gene, species: Species) = Plant(
		species.chromosomeLengths.map(length => Chromosome.ofGenes(gene, length)),
		species
	)
}



object PlantBreeding extends App{
	val species = Species(100,200,300)
	
	val pVGene = new Gene("PreferredVariety")
	val d1Gene = new Gene("Donor1")
	val d2Gene = new Gene("Donor2")
	
	
	val traits = Seq(
			Trait(0,20, d1Gene, species),
			Trait(0,60, d1Gene, species),
			Trait(1,160, d2Gene, species),
			Trait(1,20, d1Gene, species)
			//No contribution on third chromosome
	)
	
	val pV = Plant.ofGenes(pVGene, species)
	val donor1 = Plant.ofGenes(d1Gene, species)
	val donor2 = Plant.ofGenes(d2Gene, species)
	
	def cross (a: Samplable[Plant], b: Samplable[Plant], traits: Seq[Trait]): Samplable[Plant] = {
		a.combine(b, (p1: Plant,p2: Plant) => p1 x p2).filter(_.selectFor(traits))
	}
	
	implicit def toSamplabe(p: Plant) = Samplable.diracDelta(p)
	implicit val r = Random

	val cross1 = cross(pV, donor1, traits.filter(_.gene == d1Gene))
	val cross2 = cross(cross1, donor2, traits)
	
	val result = (0 to 100000).map(_ => cross2.sample.countOf(pVGene).toDouble / species.size).toEmpiricalSeq
	println(StatisticsComponent.mean(result))
}
