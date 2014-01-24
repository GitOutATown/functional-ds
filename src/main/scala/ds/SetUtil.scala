package ds

import RBT._
import scala.xml.NodeSeq

object SetUtil {

  case class AugmentedData[T : Orderable, U](v: T, data: U) extends Ordered[AugmentedData[T, U]] {
    def compare(that: AugmentedData[T, U]): Int = this.v.compare(that.v)
  }

  def toPos[T : Orderable](node: RBT[T], d: Int = 80, start: Int = 30) : RBT[AugmentedData[T, (Int, Int)]] = {
    def calcPos(n: RBT[T], x: Int, y: Int) : (RBT[AugmentedData[T, (Int, Int)]], Int) = {
      n match {
        case Leaf => (Leaf, x)
        case Node(c, v, left, right) =>
          val (leftTree, myX) = calcPos(left, x, y+d)
          val (rightTree, nextX) = calcPos(right, myX+d, y+d)
          (Node(c, AugmentedData(v, (myX, y)), leftTree, rightTree), nextX)
      }
    }
    calcPos(node, start, start)._1
  }

  def toSvg[T : Orderable](node: RBT[AugmentedData[T, (Int, Int)]], width : Int = 640, height : Int = 480): NodeSeq = {

    val rgb = Map[Color, String](Black -> "#000000", Red -> "#7f0000")

    def genNode(n: RBT[AugmentedData[T, (Int, Int)]], x: Int, y: Int): NodeSeq = n match {
      case Node(_, AugmentedData(_, (lx, ly)), _, _) =>
          <line id="svg_1" y2={y.toString} x2={x.toString} y1={ly.toString} x1={lx.toString} stroke-width="3" stroke="#000000" fill="none"/> ++
          genSvg(n)
      case Leaf => <!-- leaf -->
    }

    def genSvg(n: RBT[AugmentedData[T, (Int, Int)]]) : NodeSeq = n match {
      case Node(c, AugmentedData(t, (x, y)), left, right) =>
        genNode(left, x, y) ++
        genNode(right, x, y) ++
        <ellipse stroke={rgb(c)} fill={rgb(c)} cy={y.toString} cx={x.toString} id={"svg"+x.toString+y.toString} ry="20" rx="20" stroke-width="5"/> ++
        <text xml:space="preserve" text-anchor="middle" font-family="serif" font-size="24" id="svgt" y={(y+7).toString} x={x.toString} stroke-width="0" stroke="#ffffff" fill="#ffffff">{t}</text>
      case Leaf => <!-- leaf -->
    }
    <svg width={width.toString} height={height.toString} xmlns="http://www.w3.org/2000/svg">
      <g>
        <title>RBT</title>
        {genSvg(node)}
      </g>
    </svg>
  }
}
