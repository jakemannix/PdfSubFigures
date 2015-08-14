package org.allenai.pdfsubfigures.dissect

import java.awt.image.BufferedImage

import org.allenai.pdfsubfigures.geometry.{Split, Box}

class RecursiveDissector(img: BufferedImage) {

  def bestSplit(splits: List[Split], box: Box) : Option[Split] = {
    val features = splits.map(x => (x, new FeatureVector(x, box)))
    val score = features.map(x => (x._1, x._2.score()))
    val threshold = 10
    val candidate = score.maxBy(_._2)
    if (candidate._2 > threshold) Some(candidate._1) else None
  }

  def subFiguresFor(box: Box, split: Split): (Box, Box) = {
    if (split.isVertical) {
      (box.copy(xEnd = split.start), box.copy(xStart = split.end))
    } else {
      (box.copy(yEnd = split.start), box.copy(yStart = split.end))
    }
  }

  def findPossibleSplit(box: Box): List[Split] = {
    val dissector = new PngDissector(img)
    val guesses = dissector.findSplitGuesses(box)
    guesses
  }

  def split(box: Box): List[Box] = {
    val bestSplitFound = bestSplit(findPossibleSplit(box), box).map(split => subFiguresFor(box, split))
    bestSplitFound.map { case (box1, box2) => split(box1) ++ split(box2) }.getOrElse(List(box))
  }
}
