package org.allenai.pdfsubfigures.dissect

import java.awt.image.BufferedImage

import org.allenai.pdfsubfigures.geometry.{Split, Box}

class RecursiveDissector(img: BufferedImage) {

  def findBestSplit(box: Box): Option[Split] = {
    ???
  }

  def subFiguresFor(box: Box, split: Split): (Box, Box) = {
    if (split.isVertical) {
      (box.copy(xEnd = split.start), box.copy(xStart = split.end))
    } else {
      (box.copy(yEnd = split.start), box.copy(yStart = split.end))
    }
  }

  def split(box: Box): List[Box] = {
    val bestSplit = findBestSplit(box).map(split => subFiguresFor(box, split))
    bestSplit.map { case (box1, box2) => split(box1) ++ split(box2) }.getOrElse(List(box))
  }
}
