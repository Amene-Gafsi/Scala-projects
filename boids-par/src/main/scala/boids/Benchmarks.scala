package boids

import scala.collection.parallel.CollectionConverters.VectorIsParallelizable
import scala.util.Random
import org.openjdk.jmh.annotations.Benchmark

/** Supporting elements for benchmarks
  */
object Benchmarks:

  val seed = 0x0ddba11
  Random.setSeed(seed)

  val defaultPhysics = Physics(
    minimumSpeed = 2f,
    maximumSpeed = 5f,
    perceptionRadius = 80f,
    avoidanceRadius = 15f,
    avoidanceWeight = 1f,
    cohesionWeight = 0.001f,
    alignmentWeight = 0.027f,
    containmentWeight = 0.5f
  )

  val random1k = World.createRandom(1000, defaultPhysics)

end Benchmarks

/** Class for adding JMH benchmarks */
class Benchmarks:
  @Benchmark
  def parallelBenchmark =
    tickWorld(Benchmarks.random1k, Benchmarks.defaultPhysics)

  @Benchmark
  def sequentialBenchmark =
    // Warning: this sequential baseline is only valid if tickWorld is the only
    // parallel function.  If tickWorldSequential calls other functions and you
    // make those parallel, then they will be called (in parallelized form) and
    // benchmarked from here as well!
    tickWorldSequential(Benchmarks.random1k, Benchmarks.defaultPhysics)
