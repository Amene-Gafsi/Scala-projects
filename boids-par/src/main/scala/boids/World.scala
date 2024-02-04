package boids

import cs214.{BoidNil, BoidCons, BoidSequence, Vector2}
import scala.util.Random
import boids.conversions.FluidBoidVecs.given

class World(val physics: Physics):
  def tick(boids: Vector[Boid]): Vector[Boid] = tickWorld(boids, physics)

object World:
  def createRandomBoid(physics: Physics): Boid =
    val x = Random.between(0f, physics.WIDTH.toFloat)
    val y = Random.between(0f, physics.HEIGHT.toFloat)
    val rotation = Random.between(0f, 2 * Math.PI.toFloat)
    val initialSpeed = Random.between(physics.minimumSpeed, physics.maximumSpeed)
    val initialVelocity = Vector2.UnitUp.rotate(rotation) * initialSpeed
    Boid(position = Vector2(x, y), velocity = initialVelocity)

  def createRandom(numBoids: Int, physics: Physics): Vector[Boid] =
    (0 until numBoids).foldLeft[Vector[Boid]](BoidNil()) { (seq, _) =>
      BoidCons(createRandomBoid(physics), seq)
    }

case class Physics(
    minimumSpeed: Float,
    maximumSpeed: Float,
    perceptionRadius: Float,
    avoidanceRadius: Float,
    avoidanceWeight: Float,
    cohesionWeight: Float,
    alignmentWeight: Float,
    containmentWeight: Float
):
  val WIDTH = 1000
  val HEIGHT = 700
