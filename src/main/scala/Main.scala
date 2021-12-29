import scala.collection.mutable.ArrayBuffer

import scalatags.Text.all._
import scalatags.Text.svgTags._
import scalatags.Text.svgAttrs._

import felix.math.{Quadric, Circle, Group}
import felix.generators.RealSchottky
import felix.diagram.Bounded.given

val colors = IndexedSeq(
  "darkblue",
  "royalblue",
  "skyblue",
  "turqoise",
)

val imgWidth = 600

def shottkyDiagram(g: Group, circles: Map[Char, Circle], depth: Int): Tag =
    val bbox = circles.map(_._2.bound).reduce((a,b) => a join b)
    
    val diagram = ArrayBuffer[Tag]()

    def draw(word: String, q: Quadric) = 
        // println(word);
        val color = colors(word.length)
        diagram.append(
            (q: @unchecked) match {
                case Circle(center, radius) => circle(
                    cx:=center.realpart, cy:=center.imagpart, r:=radius, 
                    fill:=color)
            }
        )


    for (c, _) <- g.generators do
        val circle = circles(c)
        g.walk(depth, c.toString)(
            (word, elt) =>  draw(word, elt(circle))
        )
    
    val view = s"${bbox.lowerLeft._1} ${bbox.upperRight._2} ${bbox.width} ${bbox.height}"
    svg(
      scalatags.Text.svgAttrs.attr("version"):="1.1",
      scalatags.Text.svgAttrs.attr("xmlns"):="http://www.w3.org/2000/svg",
      widthA:=imgWidth,
      viewBox:=view
      )(diagram.toArray)

@main def hello: Unit =
  val (g, circles) = RealSchottky(0.7, 0.9)
  println(shottkyDiagram(g, circles, depth=0))

