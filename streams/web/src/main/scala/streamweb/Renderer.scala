package streamweb

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import org.scalajs.dom.WebGLRenderingContext
import org.scalajs.dom.WebGLRenderingContext.*
import scala.scalajs.js.typedarray.Float32Array
import scala.scalajs.js
import org.scalajs.dom.WebGLBuffer
import streamweb.Objects3D.Object3D
import scala.scalajs.js.typedarray.Uint16Array
import scala.scalajs.js.JSConverters.*
import streamweb.Objects3D.Vec3D
import scala.scalajs.js.typedarray.Uint8Array
import org.scalajs.dom.Image
import scala.scalajs.js.annotation.*

class Renderer(gl: WebGLRenderingContext):
  if gl == null then throw Exception("Unable to initialize WebGL")

  val shaderProgram = makeShaderProgram()

  val textures = Map(
    "diamond" -> loadTexture(
      gl,
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAOvwAADr8BOAVTJAAAABl0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMC4yMfEgaZUAAAAVUExURUdskU+BrVTB5VaYxFes1l7X8orx/GB2HL4AAABsSURBVAgdDcEBFcIwDAXA3zcDC0UAL52BLDFA8x2wOmD4l0DuoEe6iAQeOtODL8jvXksnIF96zNFgN9OpAlkXydlh1zqo2mEfatlgw92pG2x4mQ2WQfezwVg8ADlJZgCSqszY8dxR5A0DpPQ/564Wc8L018QAAAAASUVORK5CYII="
    ),
    "stone" -> loadTexture(
      gl,
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAAYdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjEuNWRHWFIAAAAYUExURVpPTGJWUnBfWXhoYoBya4l+dpKJgqOZlNgyU1wAAABySURBVAgdBcHBEQFBEIbR7+/tozK9AtiqnQjIQUySsknIwVnhTOm5s+M9nZJOaho2gRq7q//OiLC3EZRgloeOJFG88b2EHdIK1NqRCW0fUAwSoBlg+0Y4cOs9cLDKR3i3EeodazOQE16WUcT69JcowtY/KuYgLF7E/icAAAAASUVORK5CYII="
    ),
    "gold" -> loadTexture(
      gl,
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAOwQAADsEBuJFr7QAAABl0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMC4yMfEgaZUAAAAVUExURY5YKaBnK690K7yCK86WJ+W1MPfPTAx9E0IAAABwSURBVAgdHcHREYMgEEDBl1TgSQMGZ/KPgA1wFJDBswOv/xJisovb3+Dt7qf7h7V0zZYTIfej1bEx8yMzCw+RGCP19LFfNgjatqI5EYpuq2piQmRCBLdDVbux++2yQahHK9oSIZvmookFEOEJEm8vvhEaGglQ47S8AAAAAElFTkSuQmCC"
    ),
    "wood" -> loadTexture(
      gl,
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAOwgAADsIBFShKgAAAABh0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMS41ZEdYUgAAABhQTFRFTEA3XktBa1FEdVdGhWNMh2RNk21SoHdVw2gdMQAAAIBJREFUCB0FwUEKwjAQBdDfgHuHkq7TegFRmn0p1HUIiesyzswBgvH6vgcMdCUiwC3r/slLgIsm3EuA07azyYzxiJyrJoy8Svxqgu+Z1eoJ33V7Wblj4hJ8fgKIth/xfWISbcp1g7ci2ixh1F/jGme4Q2VSDnC3Ui/5EQAQDUT4AwTFH/UtSHprAAAAAElFTkSuQmCC"
    )
  )

  // Flip image pixels into the bottom-to-top order that WebGL expects.
  gl.pixelStorei(UNPACK_FLIP_Y_WEBGL, 1);

  def makeShaderProgram() =
    val vsSource = """
        attribute vec4 aVertexPosition;
        attribute vec2 aTextureCoord;

        uniform mat4 uModelViewMatrix;
        uniform mat4 uProjectionMatrix;

        varying highp vec2 vTextureCoord;

        void main(void) {
          gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPosition;
          vTextureCoord = aTextureCoord;
        }
        """

    val fsSource = """
        varying highp vec2 vTextureCoord;

        uniform sampler2D uSampler;

        void main(void) {
          gl_FragColor = texture2D(uSampler, vTextureCoord);
        }
        """

    val vertexShader = loadShader(gl, VERTEX_SHADER, vsSource);
    val fragmentShader = loadShader(gl, FRAGMENT_SHADER, fsSource);
    val shaderProgram = gl.createProgram();
    gl.attachShader(shaderProgram, vertexShader);
    gl.attachShader(shaderProgram, fragmentShader);
    gl.linkProgram(shaderProgram);

    if !gl.getProgramParameter(shaderProgram, LINK_STATUS).asInstanceOf[Boolean]
    then
      throw Exception(
        f"Unable to initialize the shader program: ${gl.getProgramInfoLog(shaderProgram)}"
      )

    shaderProgram

  def loadShader(gl: WebGLRenderingContext, tpe: Int, source: String) =
    val shader = gl.createShader(tpe)
    gl.shaderSource(shader, source)
    gl.compileShader(shader)

    if !gl.getShaderParameter(shader, COMPILE_STATUS).asInstanceOf[Boolean] then
      throw Exception(f"An error occured: ${gl.getShaderInfoLog(shader)}")

    shader

  def loadTexture(gl: WebGLRenderingContext, url: String) =
    val texture = gl.createTexture();
    gl.bindTexture(TEXTURE_2D, texture);

    val level = 0;
    val internalFormat = RGBA;
    val width = 1;
    val height = 1;
    val border = 0;
    val srcFormat = RGBA;
    val srcType = UNSIGNED_BYTE;
    val pixel = Uint8Array(Seq[Short](0, 0, 255, 255).toJSArray); // opaque blue
    gl.texImage2D(
      TEXTURE_2D,
      level,
      internalFormat,
      width,
      height,
      border,
      srcFormat,
      srcType,
      pixel
    );

    val image = new Image()
    image.onload = (e) =>
      gl.bindTexture(TEXTURE_2D, texture);
      gl.texImage2D(
        TEXTURE_2D,
        level,
        internalFormat,
        srcFormat,
        srcType,
        image
      );

      gl.generateMipmap(TEXTURE_2D)

      gl.texParameterf(TEXTURE_2D, TEXTURE_MAG_FILTER, NEAREST);
      gl.texParameteri(
        TEXTURE_2D,
        TEXTURE_MIN_FILTER,
        NEAREST
      );

    image.src = url;

    texture

  def drawObject(obj: Object3D, projectionM: Mat4, modelViewM: Mat4) =
    val (pos, indices, coords) = getBuffers(obj)

    // set by-vertex attributes
    setAttribute(pos, "aVertexPosition", 3) // pos have 3 components
    setAttribute(coords, "aTextureCoord", 2)
    // setAttribute(colors, "aVertexColor", 4) // colors are RGBA

    gl.bindBuffer(ELEMENT_ARRAY_BUFFER, indices)
    gl.useProgram(shaderProgram)

    // set uniform (common to all vertices) attributes
    gl.uniformMatrix4fv(
      gl.getUniformLocation(shaderProgram, "uProjectionMatrix"),
      false,
      Float32Array(projectionM.asInstanceOf[js.Array[Float]])
    )
    gl.uniformMatrix4fv(
      gl.getUniformLocation(shaderProgram, "uModelViewMatrix"),
      false,
      Float32Array(modelViewM.asInstanceOf[js.Array[Float]])
    )

    gl.activeTexture(TEXTURE0);
    gl.bindTexture(TEXTURE_2D, textures(obj.texture));
    gl.uniform1i(gl.getUniformLocation(shaderProgram, "uSampler"), 0);

    gl.drawElements(TRIANGLES, obj.faces.size, UNSIGNED_SHORT, 0);

  def setAttribute(buffer: WebGLBuffer, name: String, size: Int): Unit =
    gl.bindBuffer(ARRAY_BUFFER, buffer)
    val location = gl.getAttribLocation(shaderProgram, name)
    gl.vertexAttribPointer(location, size, FLOAT, false, 0, 0)
    gl.enableVertexAttribArray(location)

  def getBuffers(obj: Object3D): (WebGLBuffer, WebGLBuffer, WebGLBuffer) =
    val positions = new Float32Array(
      obj.vertices.flatMap(v => Seq(v._1, v._2, v._3)).toJSArray
    )
    val faces = new Uint16Array(obj.faces.toJSArray)
    val coords = new Float32Array(
      obj.coords
        .flatMap(c => Seq(c.x, c.y))
        .toJSArray
    )

    val positionsBuffer = gl.createBuffer()
    gl.bindBuffer(ARRAY_BUFFER, positionsBuffer)
    gl.bufferData(ARRAY_BUFFER, positions, STATIC_DRAW)

    val faceBuffer = gl.createBuffer()
    gl.bindBuffer(ELEMENT_ARRAY_BUFFER, faceBuffer)
    gl.bufferData(ELEMENT_ARRAY_BUFFER, faces, STATIC_DRAW)

    // TODO update
    val textureCoordBuffer = gl.createBuffer()
    gl.bindBuffer(ARRAY_BUFFER, textureCoordBuffer)
    gl.bufferData(ARRAY_BUFFER, coords, STATIC_DRAW)

    (positionsBuffer, faceBuffer, textureCoordBuffer)

  def drawScene(objects: Seq[Object3D], center: Vec3D) =
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clearDepth(1.0);
    gl.enable(DEPTH_TEST);
    gl.depthFunc(LEQUAL); // Near things obscure far things

    gl.clear(COLOR_BUFFER_BIT | DEPTH_BUFFER_BIT)

    val fieldOfView = (30 * Math.PI.toFloat) / 180; // in radians
    val aspect = gl.canvas.clientWidth.toFloat / gl.canvas.clientHeight;
    val zNear = 0.1f;
    val zFar = 100.0f;
    val projectionMatrix = Mat4.create()

    Mat4.perspective(projectionMatrix, fieldOfView, aspect, zNear, zFar);

    val modelViewMatrix = Mat4.create();

    Mat4.translate(
      modelViewMatrix,
      modelViewMatrix,
      js.Array(0.0f, 0.0f, -15.0f).asInstanceOf[Vec3]
    )

    Mat4.rotate(
      modelViewMatrix,
      modelViewMatrix,
      -1.0f,
      js.Array(1.0f, 0.0f, 0.0f).asInstanceOf[Vec3]
    )

    Mat4.rotate(
      modelViewMatrix,
      modelViewMatrix,
      -Math.PI.toFloat / 2,
      js.Array(0.0f, 0.0f, 1.0f).asInstanceOf[Vec3]
    )

    Mat4.translate(
      modelViewMatrix,
      modelViewMatrix,
      js.Array(-center.x, -center.y, center.z).asInstanceOf[Vec3]
    )

    for o <- objects do
      drawObject(o, projectionMatrix, modelViewMatrix)
