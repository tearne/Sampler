package farrington.core.simulate

case class SimulationParams(
    alpha: Double,
    beta: Double,
    m: Int,
    gamma_1: Double,
    gamma_2: Double,
    dispersion: Double
  )
case object SimulationParams{
  val default = SimulationParams(1.5, 0, 1, 0.2, -0.4, 1)
}