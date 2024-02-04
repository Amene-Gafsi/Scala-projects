package streamweb

object Objects3D:
  object Object3D:
    val Box = Object3D(
      Seq(
        (0, 0, 1),
        (1, 0, 1),
        (1, 1, 1),
        (0, 1, 1),
        (0, 0, 0),
        (0, 1, 0),
        (1, 1, 0),
        (1, 0, 0),
        (0, 1, 0),
        (0, 1, 1),
        (1, 1, 1),
        (1, 1, 0),
        (0, 0, 0),
        (1, 0, 0),
        (1, 0, 1),
        (0, 0, 1),
        (1, 0, 0),
        (1, 1, 0),
        (1, 1, 1),
        (1, 0, 1),
        (0, 0, 0),
        (0, 0, 1),
        (0, 1, 1),
        (0, 1, 0)
      ).map(t => Vec3D(t._1.toFloat, t._2.toFloat, t._3.toFloat)),
      Seq(
        0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7, 8, 9, 10, 8, 10, 11, 12, 13, 14, 12,
        14, 15, 16, 17, 18, 16, 18, 19, 20, 21, 22, 20, 22, 23
      ),
      Seq.fill(6)(Seq(
        Vec2D(0.0, 0.0),
        Vec2D(1.0, 0.0),
        Vec2D(1.0, 1.0),
        Vec2D(0.0, 1.0)
      )).flatten,
      ""
      // Seq.fill(24)(Color(1.0, 0.0, 0.0, 1.0))
    )

  case class Object3D(
      vertices: Seq[Vec3D],
      faces: Seq[Int],
      coords: Seq[Vec2D],
      texture: String
  ):
    def scale(v: Vec3D) = this.copy(vertices = vertices.map(_ * v))
    def translate(v: Vec3D) = this.copy(vertices = vertices.map(_ + v))
    def rotate(axis: Vec3D, angle: Float) = ???
    def withTexture(texture: String) = this.copy(texture = texture)

  // case class Color(r: Float, g: Float, b: Float, a: Float)

  case class Vec3D(x: Float, y: Float, z: Float):
    def +(other: Vec3D) = Vec3D(x + other.x, y + other.y, z + other.z)
    def *(other: Vec3D) = Vec3D(x * other.x, y * other.y, z * other.z)

  case class Vec2D(x: Float, y: Float)
