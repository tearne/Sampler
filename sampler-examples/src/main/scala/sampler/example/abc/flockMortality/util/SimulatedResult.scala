package sampler.example.abc.flockMortality.util

case class SimulatedResult(dayStates: IndexedSeq[Map[Int, ODEState]], observed: IndexedSeq[Observed], params: Parameters) {
  
  val sheds = (0 until observed.length)
  val days = observed.map( i => (0 until i.days.length) )
    
  def distanceToObserved: Double = {
  
    def normalise(data: IndexedSeq[IndexedSeq[Double]], value: IndexedSeq[Double]) = {
      (0 until value.size).map(i => data(i).map(j => j.toDouble / value(i)))
    }
  
    val simEggs = sheds.map(i => days(i).map(j => dayStates(i)(j).eggs.toDouble))
    val obsEggs = sheds.map(i => observed(i).eggs.map(j => j.toDouble))
    
    val accumulatedSimDead = sheds.map(i => days(i).map(j => dayStates(i)(j).d))    
    val dailySimDead = sheds.map{ shed =>
      (0 until accumulatedSimDead(shed).size).map{ day => 
        if (day == 0) accumulatedSimDead(shed)(day)
        else accumulatedSimDead(shed)(day) - accumulatedSimDead(shed)(day-1)
      }
    }
    
    val dailyObsDead = sheds.map(i => observed(i).dead.map(_.toDouble))
    val accumulatedObsDead = dailyObsDead.map(i => i.scanLeft(0.0){case (a, v) => a + v}.tail)
  
    // Fit to daily mortality
    val eggsMax = obsEggs.map(shed => shed.max)
    val deadMax = dailyObsDead.map(shed => shed.max)
    val newSimDead = normalise(dailySimDead, deadMax)
    val newObsDead = normalise(dailyObsDead, deadMax)
    val newSimEggs = normalise(simEggs, eggsMax)
    val newObsEggs = normalise(obsEggs, eggsMax)
    
//    Weighted metric (sum of square root of error, weighted more strongly from start to peak of mortality and from first drop in egg production):
    val minDayEggs = sheds.map(shed => observed(shed).deadStart - 1)
    val minDayDead = sheds.map(shed => observed(shed).deadStart - 1)
    val maxDayDead = sheds.map(shed => observed(shed).deadPeak + 1)
    
    val error =
      sheds.map{ shed =>
        days(shed).foldLeft(0.0){case (acc, dayIndex) =>
          val delta =
            if (newObsEggs(shed)(dayIndex) < 0) {
              val deadVal =
                if (dayIndex >= minDayDead(shed) && dayIndex <= maxDayDead(shed))
                  1 * math.pow(math.abs(newSimDead(shed)(dayIndex) - newObsDead(shed)(dayIndex)), 0.5)   
                else
                  0.5 * math.pow(math.abs(newSimDead(shed)(dayIndex) - newObsDead(shed)(dayIndex)), 0.5)    
              deadVal
            }
            else {
              val eggVal =
                if (dayIndex >= minDayEggs(shed))
                  2 * math.pow(math.abs(newSimEggs(shed)(dayIndex) - newObsEggs(shed)(dayIndex)), 0.5)
                else
                  1 * math.pow(math.abs(newSimEggs(shed)(dayIndex) - newObsEggs(shed)(dayIndex)), 0.5)
              val deadVal =
                if (dayIndex >= minDayDead(shed) && dayIndex <= maxDayDead(shed))
                  1 * math.pow(math.abs(newSimDead(shed)(dayIndex) - newObsDead(shed)(dayIndex)), 0.5)   
                else
                  0.5 * math.pow(math.abs(newSimDead(shed)(dayIndex) - newObsDead(shed)(dayIndex)), 0.5)  
              eggVal + deadVal  
            }
          acc + delta
        }
      }  
          
    error.sum
    
  }
  
}