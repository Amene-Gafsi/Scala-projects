package boids.conversions

import cs214.Vector2

// Since Vector2Sequence had in built numeric functions, it had information
// about Vector2's numeric properties. If we use Vector[Vector2], we have to
// provide this information ourselves with a Numeric typeclass.
object NumericVectors:
  given Numeric[Vector2] with
    def fromInt(x: Int): Vector2 = Vector2(x.toFloat, 0)
    def compare(x: Vector2, y: Vector2): Int = x.norm.compare(y.norm) // shady
    def minus(x: Vector2, y: Vector2): Vector2 = x - y
    def negate(x: Vector2): Vector2 = -x
    def parseString(str: String): Option[Vector2] = str.toIntOption.map(fromInt(_))
    def plus(x: Vector2, y: Vector2): Vector2 = x + y
    def times(x: Vector2, y: Vector2): Vector2 = Vector2(x.x * y.x, x.y * y.y) // shady
    def toDouble(x: Vector2): Double = x.norm
    def toFloat(x: Vector2): Float = x.norm
    def toInt(x: Vector2): Int = x.norm.toInt
    def toLong(x: Vector2): Long = x.norm.toLong
