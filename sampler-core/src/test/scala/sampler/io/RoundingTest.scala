package sampler.io

import org.scalatest.FreeSpec


class RoundingTest extends FreeSpec {
  "do decimal places" in {
    pending()
  }

  "Significant figures" in new Rounding {
    assert(1234567890.significanatFigures(3) === 1230000000)
  }
}
