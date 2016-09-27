package sampler.abc.refactor

import sampler.abc.Generation

/**
  * Created by user on 9/16/16.
  */
object Messages {
  case class Start[P](initGen: Generation[P])
  case class FlushCompleteTerminate[P](state: StateData[P])
  case class FlushCompleteContinue[P](state: StateData[P])
}
