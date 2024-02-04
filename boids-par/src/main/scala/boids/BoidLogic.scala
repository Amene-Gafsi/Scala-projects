package boids
import cs214.{Vector2, BoidSequence}
import scala.collection.parallel.CollectionConverters.* // access to .par methods
import boids.conversions.NumericVectors.given

def boidsWithinRadius(thisBoid: Boid, boids: Vector[Boid], radius: Float): Vector[Boid] =
  boids.par.filter(b =>
    b != thisBoid &&
      b.position.distanceTo(thisBoid.position) < radius
  ).seq

def avoidanceForce(thisBoid: Boid, boidsWithinAvoidanceRadius: Vector[Boid]): cs214.Vector2 =
  boidsWithinAvoidanceRadius.par
    .map(b => thisBoid.position - b.position)
    .filter(_.norm > 0)
    .map(diff => diff / (diff.norm * diff.norm))
    .sum

def cohesionForce(thisBoid: Boid, boidsWithinPerceptionRadius: Vector[Boid]): cs214.Vector2 =
  if boidsWithinPerceptionRadius.isEmpty then
    Vector2.Zero
  else
    boidsWithinPerceptionRadius.par.map(_.position).sum
      / boidsWithinPerceptionRadius.length.toFloat
      - thisBoid.position

def alignmentForce(thisBoid: Boid, boidsWithinPerceptionRadius: Vector[Boid]): cs214.Vector2 =
  if boidsWithinPerceptionRadius.isEmpty then
    Vector2.Zero
  else
    boidsWithinPerceptionRadius.par.map(_.velocity).sum
      / boidsWithinPerceptionRadius.length.toFloat
      - thisBoid.velocity

def containmentForce(thisBoid: Boid, allBoids: Vector[Boid], width: Int, height: Int): cs214.Vector2 =
  val horizontalForce =
    if thisBoid.position.x < 0 then Vector2.UnitRight
    else if thisBoid.position.x > width then Vector2.UnitLeft
    else Vector2.Zero
  val verticalForce =
    if thisBoid.position.y < 0 then Vector2.UnitDown
    else if thisBoid.position.y > height then Vector2.UnitUp
    else Vector2.Zero
  horizontalForce + verticalForce

def totalForce(thisBoid: Boid, allBoids: Vector[Boid], physics: Physics): Vector2 =
  val withinPerceptionRadius = boidsWithinRadius(thisBoid, allBoids, physics.perceptionRadius)
  val cohere = cohesionForce(thisBoid, withinPerceptionRadius)
  val align = alignmentForce(thisBoid, withinPerceptionRadius)
  val withinAvoidanceRadius = boidsWithinRadius(thisBoid, withinPerceptionRadius, physics.avoidanceRadius)
  val avoid = avoidanceForce(thisBoid, withinAvoidanceRadius)
  val contain = containmentForce(thisBoid, allBoids, physics.WIDTH, physics.HEIGHT)
  val total =
    avoid * physics.avoidanceWeight +
      cohere * physics.cohesionWeight +
      align * physics.alignmentWeight +
      contain * physics.containmentWeight
  total

def clampVelocity(velocity: cs214.Vector2, minimumSpeed: Float, maximumSpeed: Float): cs214.Vector2 =
  if velocity.norm < minimumSpeed then
    velocity.normalized * minimumSpeed
  else if velocity.norm > maximumSpeed then
    velocity.normalized * maximumSpeed
  else
    velocity

def tickBoid(thisBoid: Boid, allBoids: Vector[Boid], physics: Physics): Boid =
  val acceleration = totalForce(thisBoid, allBoids, physics)
  Boid(
    thisBoid.position + thisBoid.velocity,
    clampVelocity(
      thisBoid.velocity + acceleration,
      physics.minimumSpeed,
      physics.maximumSpeed
    )
  )

def tickWorld(allBoids: Vector[Boid], physics: Physics): Vector[Boid] =
  allBoids.par.map(boid => tickBoid(boid, allBoids, physics)).seq

// Port this function to use Vector[Boid], but otherwise leave it untouched
// (don't add parallelism): it serves as a baseline in the benchmarks.
def tickWorldSequential(allBoids: Vector[Boid], physics: Physics): Vector[Boid] =
  allBoids.map(boid => tickBoid(boid, allBoids, physics))
