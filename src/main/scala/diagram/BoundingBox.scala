package felix.diagram

import felix.math.{Complex, Quadric, Circle, Line}

final case class BoundingBox(x: Double, y: Double, width: Double, height: Double):
    require(width > 0 && height > 0)

    def lowerLeft: (Double, Double) = (x,y)
    def upperRight: (Double, Double) = (x+width, y+height)
    def center: (Double, Double) = (x+width/2, y+width/2)

    def aspectRatio: Double = height/width

    def join(that: BoundingBox): BoundingBox =
        val (x00,y00) = this.lowerLeft
        val (x01,y01) = that.lowerLeft
        val (x10,y10) = this.upperRight
        val (x11,y11) = that.upperRight
        val xmin = x00 min x01
        val ymin = y00 min y01        
        BoundingBox(xmin, ymin, (x10 max x11) - xmin, (y10 max y11) - ymin)

    def + (pt: Complex): BoundingBox =
        BoundingBox(this.x + pt.realpart, this.y + pt.imagpart, this.width, this.height)

    def scaleAtCenter(scale: Double): BoundingBox =
        val (cx, cy) = center
        val dx = (x-cx)*scale
        val dy = (y-cy)*scale        
        BoundingBox(cx+dx, cy+dy, -2*dx, -2*dy)

object BoundingBox {
    val MaximalBox = BoundingBox(Double.MinValue, Double.MinValue, Double.MaxValue, Double.MaxValue)
}

trait Bounded[A]:
    extension(a: A) def bound: BoundingBox

object Bounded:
    given Bounded[Quadric] with
        extension(q: Quadric) def bound = q match {
            case Circle(center, radius)=> {
                BoundingBox(center.realpart-radius, center.imagpart-radius, 2*radius, 2*radius)
            }
            case l: Line => BoundingBox.MaximalBox
        }