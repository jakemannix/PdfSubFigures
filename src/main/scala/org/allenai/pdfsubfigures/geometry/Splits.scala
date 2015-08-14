package org.allenai.pdfsubfigures.geometry

import java.awt.image.BufferedImage

case class Split(start: Int, end: Int, isVertical: Boolean) {
  val width = end - start
}

case class Box(xStart: Int, yStart: Int, xEnd: Int, yEnd: Int) {
  val height = yEnd - yStart
  val width = xEnd - xStart
}
