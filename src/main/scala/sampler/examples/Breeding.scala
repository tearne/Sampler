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
import sampler.r.QuickPlot
import java.nio.file.Paths
import java.nio.file.Files


/*
 * Given a breeding heirarchy where donor traits are to be 
 * introduced (heterozygously) into a preferred variety, what 
 * proportion (distribution) of preferred variety is present 
 * in the final plant?
 */

object Breeding extends App{
	import sampler.examples.Biology._
	
	//Number of centimorgan per chromosome
	val species = Species(100,200,300)
	
	val pVGene = new Gene("PreferredVariety")
	val d1Gene = new Gene("Donor1")
	val d2Gene = new Gene("Donor2")
	
	//Traits which will be selected for
	val traits = Seq(
			Trait(0,60, d1Gene, species),
			Trait(1,160, d2Gene, species),
			Trait(1,20, d1Gene, species)
			//No contribution on third chromosome
	)
	
	
	val pV = Plant.ofGenes(pVGene, species)
	val donor1 = Plant.ofGenes(d1Gene, species)
	val donor2 = Plant.ofGenes(d2Gene, species)
	
	def cross (a: Samplable[Plant], b: Samplable[Plant], traits: Seq[Trait] = Nil): Samplable[Plant] = {
		a.combine(b, (p1: Plant, p2: Plant) => p1 x p2).filter(_.selectsFor(traits))
	}
	
	val d1d2_f1   	= cross(donor1, donor2)
	val d1d2pV_f1 	= cross(d1d2_f1, pV, traits)
	val bc1 		= cross(d1d2pV_f1, pV, traits)
	
	val proportionPreferredVariety = (0 to 10).map(i => {
		val proportion = bc1.sample.countOf(pVGene).toDouble / species.size
		println("Done "+i+" got "+proportion)
		proportion
	}).toEmpiricalSeq
	
	val wd = Paths.get("egout","Breeding")
	Files.createDirectories(wd)
	QuickPlot.writeDensity(
		wd,
		"Proportion",
		Map("PrefVar" -> proportionPreferredVariety)
	)
}

object Biology{
	implicit val r = Random
	
	case class Species(chromosomeLengths: Int*){
		val size = 2 * chromosomeLengths.sum
	}
	
	case class Locus(chromosome: Int, leftTid: Boolean, cM: Int, species: Species){
		import species._
		assert(cM >= 0 && cM <= chromosomeLengths(chromosome))
		val nextCM = if(cM  == chromosomeLengths(chromosome) - 1) None else Some(cM + 1)
	}

	class Gene(val name: String) extends AnyVal

	case class Trait(chromosome: Int, cM: Int, gene: Gene, species: Species) //Always assume heterozygous for now
	
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
		
		def selectsFor(traits: Seq[Trait]) = 
			!traits.exists(t => !satisfiesTrait(t) || t.species != species)
		
		def countOf(gene: Gene) = chromosomes.foldLeft(0)(_ + _.countOf(gene))
		
		def recombinationModel = Samplable.bernouliTrial(Probability(0.01))
		def coin = Samplable.coinToss
		
		def x(that: Plant): Plant = {
				assert(this.species == that.species)
				@tailrec
				def gameteBuilder(parent: Plant, locus: Locus, acc: Seq[Gene] = Nil): Chromatid = {
					val amOnLeftChromosome = (locus.leftTid ^ recombinationModel.sample)
					val nextGene = parent.geneAt(locus)
					
					locus.nextCM match{
						case None => 
							val allGenes = nextGene +: acc
							assert(allGenes.size == this.species.chromosomeLengths(locus.chromosome), acc.size)
							Chromatid(allGenes)
						case Some(nextCM) =>
							val nextLocus = locus.copy(leftTid = amOnLeftChromosome, cM = nextCM)
							gameteBuilder(parent, nextLocus, nextGene +: acc)
					}
				}
				
				
				val offSpringChromosomes = (0 until species.chromosomeLengths.size).map(chromosomeIdx => {
					val g1 = gameteBuilder(this, Locus(chromosomeIdx, coin.sample, 0, species))
					val g2 = gameteBuilder(that, Locus(chromosomeIdx, coin.sample, 0, species))
					Chromosome(g1,g2)
				})
				
				Plant(offSpringChromosomes, species)
			}
		
	}
	object Plant {
		def ofGenes(gene: Gene, species: Species) = {
			val chromosomes = species.chromosomeLengths.map(length => Chromosome.ofGenes(gene, length))
			Plant(
				chromosomes,
				species
			)	
		}
	}
	
	implicit class PlantAsSamplable(p: Plant) extends Samplable[Plant]{
		def sample(): Plant = p
	}
}







