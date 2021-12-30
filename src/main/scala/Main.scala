import scala.collection.mutable.ArrayBuffer

import scalatags.Text.all._
import scalatags.Text.svgTags._
import scalatags.Text.svgAttrs._

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.document

import felix.math.{Complex, Quadric, Circle, Group}
import felix.generators.RealSchottky
import felix.diagram.Bounded.given
import felix.diagram.BoundingBox
import java.awt.Canvas

trait Renderer:
  def view: BoundingBox
  def view_=(box: BoundingBox): Unit
  def clear(): Unit
  def render(word: String, q: Quadric): Unit

val colors = IndexedSeq(
  "darkblue",
  "royalblue",
  "skyblue",
  "turqoise",
)

class CanvasRenderer(val canvas: html.Canvas) extends Renderer:
  val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  private var bbox = BoundingBox(0, 0, canvas.width, canvas.height)

  def view = bbox
  def view_=(box: BoundingBox) = 
    // note: canvas has the positive y axis moving down from the upper left corner, but we otherwise
    // assume the opposite
    println(box)
    val scale = (canvas.width / box.width) min (canvas.height / box.height)
    context.setTransform(scale, 0, 0, scale, -box.x*scale, -box.y*scale)
    bbox = box

  def clear() =
    context.save()
    context.setTransform(1,0,0,1,0,0)
    context.clearRect(0, 0, canvas.width, canvas.height)
    context.restore()

  def render(word: String, q: Quadric) =
    // println(word)
    (q: @unchecked) match {
      case Circle(center, radius) => {
        context.fillStyle = colors(word.length % colors.size)
        context.beginPath()
        context.arc(center.realpart, center.imagpart, radius, 0, 2*scala.math.Pi)
        context.fill()
      }
    }


def drawShottky(g: Group, circles: Map[Char, Circle], depth: Int)(using renderer: Renderer): Unit =
  for (c, _) <- g.generators do
    val circle = circles(c)
    g.walk(depth, c.toString)(
      (word, elt) =>  renderer.render(word, elt(circle))
    )

val dxdw = 0.01
val dydh = 0.01

@main def main: Unit =
  val canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
  canvas.width = canvas.parentElement.clientWidth
  canvas.height = canvas.parentElement.clientHeight

  given renderer: Renderer = new CanvasRenderer(canvas)

  val (g, circles) = RealSchottky(0.2, 0.9)

  def draw() = 
    renderer.clear()
    drawShottky(g, circles, 3)

  renderer.clear()
  renderer.view = circles.map(_._2.bound).reduce((a,b) => a join b)

  def navigate(e: dom.KeyboardEvent): Unit = 
    e.key match
      case "a" => renderer.view = renderer.view + Complex(-dxdw*renderer.view.width, 0); draw()
      case "d" => renderer.view = renderer.view + Complex(dxdw*renderer.view.width, 0); draw()
      case "w" => renderer.view = renderer.view + Complex(0, -dydh*renderer.view.height); draw()
      case "s" => renderer.view = renderer.view + Complex(0, dydh*renderer.view.height); draw()
      case _ => ()

  def zoom(e: dom.WheelEvent): Unit = 
    val scale = 1-0.01*e.deltaY
    println(scale)
    renderer.view = renderer.view.scaleAtCenter(scale)
    draw()

  dom.window.addEventListener("keydown", navigate _)
  dom.window.addEventListener("wheel", zoom _)
  draw()

