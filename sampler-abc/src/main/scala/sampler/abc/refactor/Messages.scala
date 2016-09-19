package sampler.abc.refactor

import sampler.abc.Generation

/**
  * Created by user on 9/16/16.
  */
object Messages {
  case class Start[P](initGen: Generation[P])
}
