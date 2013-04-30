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


/*
 * Given a breeding heirarchy where donor traits are to be 
 * introduced (heterozygously) into a preferred variety, what 
 * proportion (distribution) of preferred variety is present 
 * in the final plant?
 */

case class Species(chromosomeLengths: Int*)

class Gene(val name: String) extends AnyVal

case class Trait(chromosome: Int, cM: Int, gene: Gene, species: Species) //Always assume heterozygous for now

case class Locus(chromosome: Int, leftTid: Boolean, cM: Int, species: Species){
	import species._
	assert(cM >= 0 && cM < chromosomeLengths(chromosome))
	val nextCM = if(chromosomeLengths(chromosome) == cM - 1) None else Some(cM + 1)
}

case class Chromatid(genes: Seq[Gene])
object Chromatid{
	def ofGenes(gene: Gene, length: Int) = Chromatid(Seq.fill(length)(gene))
}

case class Chromosome(left: Chromatid, right: Chromatid)
object Chromosome {
	def ofGenes(gene: Gene, length: Int) = Chromosome(
			Chromatid.ofGenes(gene, length),
			Chromatid.ofGenes(gene, length)
			)
}

case class Plant(chromosomes: Seq[Chromosome]){
	def geneAt(locus: Locus) = //TODO this is a mess
		if(locus.leftTid) chromosomes(locus.chromosome).left.genes(locus.cM)
		else chromosomes(locus.chromosome).right.genes(locus.cM)
	
	def satisfiesTrait(tr: Trait) =
		geneAt(Locus(tr.chromosome, true, tr.cM, tr.species)) == tr.gene || geneAt(Locus(tr.chromosome, false, tr.cM, tr.species)) == tr.gene
	
	def selectsFor(traits: Seq[Trait]) = 
		!traits.exists(t => !satisfiesTrait(t))
		
	def crossWith(other: Plant): Plant = {
		//check same species
		//TODO
	}
}
object Plant {
	def ofGenes(gene: Gene, species: Species) = Plant(
			species.chromosomeLengths.map(length => Chromosome.ofGenes(gene, length))
			)
}



class PlantBreeding {
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
	
	
	val PV = Plant.ofGenes(pVGene, species)
	val Donor1 = Plant.ofGenes(d1Gene, species)
	val Donor2 = Plant.ofGenes(d2Gene, species)
	
	implicit val r = Random
	def recombinationModel = Samplable.bernouliTrial(Probability(0.01))
	def coin = Samplable.coinToss

	def cross(mum: Plant, dad: Plant): Plant = {
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
			gameteBuilder(mum, Locus(chromosomeIdx, coin.sample, 0, species)),
			gameteBuilder(dad, Locus(chromosomeIdx, coin.sample, 0, species))
		))
		
		Plant(offSpringChromosomes)
	}
	
	def offspringSampler(mum: Plant, dad: Plant) = new Samplable[Plant] {
		def sample(): Plant = cross(mum, dad)
	}//.filter(_.selectsFor(traits))
	
	def d1d2Offspring = {
		
	}
}