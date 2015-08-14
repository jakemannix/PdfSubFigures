package org.allenai.pdfsubfigures.dissect

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import org.allenai.pdfsubfigures.geometry.{Split, Box}

class RecursiveDissector(img: BufferedImage) {

  def bestSplits(splits: List[Split], box: Box) : List[Split] = {

    val splitTupleLists = splits.toSet[Split].map(x => List(x)).toList ++ splits.toSet.subsets(2).map(_.toList).toList
    val features = splitTupleLists.map(x => (x, new FeatureVector(x, box, img)))
//    splits.map(x => println(x.width))
    val score = features.map(x => (x._1, x._2.score()))
    val threshold = 0.5
    val candidate = score.maxBy(_._2)
    if (candidate._2 > threshold) candidate._1 else List[Split]()
  }


  def findPossibleSplit(box: Box): List[Split] = {
    val dissector = new PngDissector(img)
    val guesses = dissector.findSplitGuesses(box)
    val (vert, horiz) = guesses.partition(_.isVertical)
    vert ++ horiz
  }

  def split(box: Box): List[Box] = {
    val possibleSplits = findPossibleSplit(box)
    val bestSplitsList = bestSplits(possibleSplits, box)
    if (bestSplitsList.nonEmpty) { // found a high scoring split, let's split on it
      RecursiveDissector.subFiguresFor(box, bestSplitsList).map(x => split(x)).flatten
    } else {
      List(box)
    }
//    val bestSplitFound = bestSplit(findPossibleSplit(box), box).map(split => subFiguresFor(box, split))
//    bestSplitFound.map { case (box1, box2) => split(box1, depth + 1) ++ split(box2, depth + 1) }.getOrElse(List(box))
  }
}


object RecursiveDissector {

  def findBoxes(pngFileName: String): List[Box] = {
    val pngDissector = new PngDissector(ImageIO.read(new File(pngFileName)))
    val img = pngDissector.img
    val otherDissector = new RecursiveDissector(pngDissector.img)
    val startBox = Box(xStart = 0, yStart = 0,
      xEnd = img.getWidth, yEnd = img.getHeight)
    otherDissector.split(startBox)
  }

  def crop(pngFileName: String)(box: Box): Box = {
    val pngDissector = new PngDissector(ImageIO.read(new File(pngFileName)))
    val img = pngDissector.img
    val otherDissector = new RecursiveDissector(pngDissector.img)
    val (vertSplits, horizSplits) = otherDissector.findPossibleSplit(box).partition(_.isVertical)
    ???
  }

  def subFiguresFor(box: Box, splits: List[Split]): List[Box] = {
    val childBoxes = for (i <- 0 to splits.length) yield {
      if (splits(0).isVertical) {
        if (i == 0) {
          box.copy(xEnd = splits(i).start)
        }
        else if (i == splits.length) {
          box.copy(xStart = splits(i - 1).end)
        }
        else {
          box.copy(xStart = splits(i - 1).end, xEnd = splits(i).start)
        }
      } else {
        if (i == 0) {
          box.copy(yEnd = splits(i).start)
        }
        else if (i == splits.length) {
          box.copy(yStart = splits(i - 1).end)
        }
        else {
          box.copy(yStart = splits(i - 1).end, yEnd = splits(i).start)
        }
      }
    }
    childBoxes.toList
  }
}