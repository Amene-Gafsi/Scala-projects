package boids
import cs214.{Vector2, BoidSequence}

def boidsWithinRadius(thisBoid: Boid, boids: BoidSequence, radius: Float): BoidSequence =
 boids.filter(boid => (thisBoid.position.distanceTo(boid.position) < radius) && (thisBoid.position.distanceTo(boid.position)>0) )  


def avoidanceForce(thisBoid: Boid, boidsWithinAvoidanceRadius: BoidSequence): cs214.Vector2 =
  if boidsWithinAvoidanceRadius.isEmpty then cs214.Vector2.Zero
  else 
    computeForce(thisBoid, boidsWithinAvoidanceRadius.head).+(avoidanceForce(thisBoid, boidsWithinAvoidanceRadius.tail))


def cohesionForce(thisBoid: Boid, boidsWithinPerceptionRadius: BoidSequence): cs214.Vector2 =
    if boidsWithinPerceptionRadius.isEmpty then cs214.Vector2.Zero
    else 
      val centerOfMass = boidsWithinPerceptionRadius.mapVector2(boid => boid.position).sum./(boidsWithinPerceptionRadius.length.toFloat)
      centerOfMass.-(thisBoid.position)


def alignmentForce(thisBoid: Boid, boidsWithinPerceptionRadius: BoidSequence): cs214.Vector2 =
    if boidsWithinPerceptionRadius.isEmpty then cs214.Vector2.Zero
    else 
      val centerOfMass = boidsWithinPerceptionRadius.mapVector2(boid => boid.velocity).sum./(boidsWithinPerceptionRadius.length.toFloat)
      centerOfMass.-(thisBoid.velocity)



def containmentForce(thisBoid: Boid, allBoids: BoidSequence, width: Int, height: Int): cs214.Vector2 =
  val xForce = if (thisBoid.position.x < 0)  1 
                else if (thisBoid.position.x > width) -1
                else 0
  val yForce = if (thisBoid.position.y < 0)  1 
                else if (thisBoid.position.y > height) -1
                else 0
  cs214.Vector2(xForce.toFloat, yForce.toFloat)

def totalForce(thisBoid: Boid, allBoids: BoidSequence, physics: Physics): Vector2 =
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


def tickBoid(thisBoid: Boid, allBoids: BoidSequence, physics: Physics): Boid =
  val acceleration = totalForce(thisBoid, allBoids, physics)
  val newVelocity = thisBoid.velocity + acceleration
  val newNorm = newVelocity.norm

  val clampedVelocity = if (newNorm > physics.maximumSpeed) newVelocity.normalized * physics.maximumSpeed
   else if (newNorm < physics.minimumSpeed)  newVelocity.normalized * physics.minimumSpeed
   else newVelocity

  Boid(thisBoid.position + thisBoid.velocity, clampedVelocity)

def tickWorld(allBoids: BoidSequence, physics: Physics): BoidSequence =
  allBoids.mapBoid(boid => tickBoid( boid, allBoids, physics))
  

def computeForce(boid1: Boid, boid2: Boid): cs214.Vector2 =
  val distance = boid1.position.distanceTo(boid2.position)

  if (distance == 0) 
    cs214.Vector2(0, 0)
  else 
    val forceX = (boid1.position.x - boid2.position.x) / math.pow(distance,2)
    val forceY = (boid1.position.y - boid2.position.y) / math.pow(distance,2)
    cs214.Vector2(forceX, forceY)

