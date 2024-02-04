package streamweb

import scalajs.js
import scalajs.js.annotation.JSName
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal
class GraphVisualization(
    nodes: js.Array[js.Dictionary[Any]],
    links: js.Array[js.Dictionary[Any]],
    background: js.Array[js.Dictionary[Any]]
) extends js.Any:
  def toggleLayout(): Unit = js.native
  def animate(
      nodes: js.Array[js.Dictionary[Any]],
      links: js.Array[js.Dictionary[Any]]
  ): Unit = js.native

// def displayGraph(
//     nodes: js.Dictionary[js.Dictionary[Any]],
//     links: js.Array[js.Dictionary[Any]],
//     background: js.Array[js.Dictionary[Any]]
// ): Unit = js.native
