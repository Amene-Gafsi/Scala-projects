package degrees

class ConvertTest extends munit.FunSuite:
  val DELTA = 0.0001

  test("celsiusToFahrenheit(0) should be 32 (1pt)"):
    assertEqualsDouble(celsiusToFahrenheit(0), 32.0, DELTA)

  test("celsiusToFahrenheit(100) should be 212 (1pt)"):
    assertEqualsDouble(celsiusToFahrenheit(100), 212.0, DELTA)

  test("fahrenheitToCelsius(32) should be 0 (1pt)"):
    assertEqualsDouble(fahrenheitToCelsius(32), 0.0, DELTA)

  test("fahrenheitToCelsius(212) should be 100 (1pt)"):
    assertEqualsDouble(fahrenheitToCelsius(212), 100.0, DELTA)

  test("celsiusToFahrenheit and fahrenheitToCelsius should be inverse functions (2pts)"):
    for v <- -100 to 100 do
      assertEqualsDouble(fahrenheitToCelsius(celsiusToFahrenheit(v)), v.toDouble, DELTA)
      assertEqualsDouble(celsiusToFahrenheit(fahrenheitToCelsius(v)), v.toDouble, DELTA)
