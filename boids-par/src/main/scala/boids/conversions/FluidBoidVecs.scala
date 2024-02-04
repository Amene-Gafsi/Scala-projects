package boids.conversions

import cs214.*
import _root_.boids.Boid
import scala.collection.mutable.Builder

// since the server code uses Vector[Boid] now, it would not compile till a
// student changes the code from BoidSequence to Vector[Boid]. So, we provide a
// conversion between the two at the call site.
object FluidBoidVecs:
  @annotation.tailrec
  def convertToVec(seq: BoidSequence, acc: Builder[Boid, Vector[Boid]] = Vector.newBuilder[Boid]): Vector[Boid] =
    seq match
      case BoidNil()            => acc.result()
      case BoidCons(head, tail) => convertToVec(tail, acc += head)

  def convertToSeq(seq: Vector[Boid]): BoidSequence =
    seq.foldLeft(BoidNil(): BoidSequence)((acc, next) => BoidCons(next, acc))

  given Conversion[BoidSequence, Vector[Boid]] = convertToVec(_)
  given Conversion[Vector[Boid], BoidSequence] = convertToSeq(_)
