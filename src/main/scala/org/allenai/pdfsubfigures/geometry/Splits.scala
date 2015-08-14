package org.allenai.pdfsubfigures.geometry

case class Split(start: Int, end: Int, isVertical: Boolean)

case class Box(xStart: Int, yStart: Int, xEnd: Int, yEnd: Int)