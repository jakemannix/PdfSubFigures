package org.allenai.pdfsubfigures.dissect

import java.awt.image.BufferedImage

import org.allenai.pdfsubfigures.geometry.{Box, Split}
import math._

class FeatureVector(val splits: List[Split], val box: Box, val img: BufferedImage) {

  val splitVertical = splits(0).isVertical

  val childBoxes = RecursiveDissector.subFiguresFor(box, splits)

  var meanBoxWidth = 0.0
  for (childBox <- childBoxes) {
    meanBoxWidth += (if (splitVertical) {childBox.width} else {childBox.height})
  }
  meanBoxWidth/=childBoxes.length

  val symmetry = childBoxes.map(x => abs(log((if (splitVertical) {x.width} else {x.height})/meanBoxWidth))).max

  val splitWidth = splits.map(_.width).sum
//  val aspectRatio1 = boxChild1.approxAspectRatio
//  val aspectRatio2 = boxChild2.approxAspectRatio
//  val maxLogAspectRatio = max(abs(log(aspectRatio1)), abs(log(aspectRatio2)))
//  val blankCoverage = split.width.toDouble / (if (split.isVertical) box.width.toDouble else box.height.toDouble)
//  val absoluteArea = min(boxChild1.width*boxChild1.height.toDouble, boxChild2.width*boxChild2.height.toDouble)
  val smallestDimension = childBoxes.map(x => (if (splitVertical) {x.width} else {x.height})).min

  val boxBrightnessList = childBoxes.map(x => PngDissector.meanPixelBrightness(img, x))

  val meanBoxBrigthness = boxBrightnessList.sum/childBoxes.length
  val maxBrightnessDifference = boxBrightnessList.map(x => abs(x - meanBoxBrigthness)).max



//  val brightnessDifference = abs(PngDissector.meanPixelBrightness(img, boxChild1)/ - PngDissector.meanPixelBrightness(img,boxChild2))

  /**
   * Given a feature vector produces a score
   * @return
   */
  def score() : Double = {

    val widthWeight = 1
    val aspectRatioWeight = 0
    val blankCoverageWeight = 0

    if (smallestDimension > 100 && symmetry < 0.4 && maxBrightnessDifference < 2) {
      if (splitWidth.toDouble == 0) {
        1
      } else {
//        (maxBrightnessDifference*(-1)+2) *
          splitWidth.toDouble
      }
    } else {
      0
    }
  }
}
