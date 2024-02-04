package streamweb

import scalajs.js
import scalajs.js.annotation.JSName
import scala.scalajs.js.annotation.JSGlobal

// @JSName("mat3")
trait Mat3 extends js.Object

// @JSName("mat4")
trait Mat4 extends js.Object

// @JSName("mat2")
trait Mat2 extends js.Object

// @JSName("vec2")
trait Vec2 extends js.Object

// @JSName("vec3")
trait Vec3 extends js.Object

// @JSName("vec4")
trait Vec4 extends js.Object

@js.native
@JSGlobal("mat3")
object Mat3 extends js.Object:
  def create(): Mat3 = js.native
  def translate(out: Mat3, a: Mat3, v: Vec2): Mat3 = js.native

@js.native
@JSGlobal("mat4")
object Mat4 extends js.Object:
  def create(): Mat4 = js.native
  def perspective(out: Mat4, fovy: Float, aspect: Float, near: Float, far: Float): Mat4 = js.native
  def translate(out: Mat4, a: Mat4, v: Vec3): Mat4 = js.native
  def rotate(out: Mat4, a: Mat4, rad: Float, axis: Vec3): Mat4 = js.native
  // def translate(out: Mat4, a: Mat4, v: js.Array[Double]): Mat4 = translate(out, a, v.asInstanceOf[Vec3])

@js.native
@JSGlobal("vec3")
object Vec3 extends js.Object:
  def create(): Vec3 = js.native
