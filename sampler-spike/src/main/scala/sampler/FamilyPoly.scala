package sampler

object FamilyPoly {
	trait House{
		type Pet <: PetBase
		protected trait PetBase{
			def feed(volume: Int): Pet
			val hunger: Double
		}
	}
	
	class Feeding[H <: House](val house: H)(){
		import house._
		def doFeed(pet: house.Pet, volumeFood: Double): Pet  = {
			pet.feed(pet.hunger.toInt)
		}
	}
	
	object HouseImpl extends House{
		case class Pet(hunger: Double) extends PetBase{
			def feed(volume: Int) = {
				val newHungryness = hunger - volume
				if(newHungryness < 0) println("VOMIT!")
				copy(hunger = newHungryness)
			}
		}
	}
	
	val p = HouseImpl.Pet(5.3)
	p.feed(5)
}