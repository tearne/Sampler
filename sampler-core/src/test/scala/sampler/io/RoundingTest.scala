package sampler.io

import org.scalatest.FreeSpec

class RoundingTest extends FreeSpec with Rounding{
  "Decimal places" in {
    assert(1234567890.decimalPlaces(3) === 1234567890)
    assert(123456.7890.decimalPlaces(3) === 123456.789)

    assert(12345.67890.decimalPlaces(3) === 12345.679)
    assert(12345.67890.decimalPlaces(2) === 12345.68)
    assert(12345.67890.decimalPlaces(1) === 12345.7)
    assert(12345.67890.decimalPlaces(0) === 12346)

    assert(1.234567890.decimalPlaces(2) === 1.23)
    assert(1.234567890.decimalPlaces(1) === 1.2)
    assert(1.234567890.decimalPlaces(0) === 1)
  }

  "Significant figures" in {
    assert(1234567890.significanatFigures(6) === 1234570000)
    assert(1234567890.significanatFigures(5) === 1234600000)
    assert(1234567890.significanatFigures(4) === 1235000000)
    assert(1234567890.significanatFigures(3) === 1230000000)
    assert(1234567890.significanatFigures(2) === 1200000000)
    assert(1234567890.significanatFigures(1) === 1000000000)

    assert(12345.67890.significanatFigures(6) === 12345.70000)
    assert(12345.67890.significanatFigures(5) === 12346.00000)
    assert(12345.67890.significanatFigures(4) === 12350.00000)
    assert(12345.67890.significanatFigures(3) === 12300.00000)
    assert(12345.67890.significanatFigures(2) === 12000.00000)
    assert(12345.67890.significanatFigures(1) === 10000.00000)
  }
}
