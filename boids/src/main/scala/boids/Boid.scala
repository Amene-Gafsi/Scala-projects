package boids

import cs214.{BoidSequence, Vector2}

case class Boid(
    position: Vector2,
    velocity: Vector2
):

  def tick(allBoids: BoidSequence, physics: Physics): Boid =
    tickBoid(this, allBoids, physics)
