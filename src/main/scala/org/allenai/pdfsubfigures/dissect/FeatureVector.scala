package org.allenai.pdfsubfigures.dissect

import org.allenai.pdfsubfigures.geometry.{Box, Split}
import math._

class FeatureVector(val split: Split, val box: Box) {


  val boxChild1 = if(split.isVertical) {
    box.copy(xEnd = split.start)
  } else {
    box.copy(yEnd = split.start)
  }
  val boxChild2 = if(split.isVertical) {
    box.copy(xStart = split.end)
  } else {
    box.copy(yStart = split.end)
  }

  val splitWidth = split.width.toDouble
  val aspectRatio1 = boxChild1.approxAspectRatio
  val aspectRatio2 = boxChild2.approxAspectRatio
  val maxLogAspectRatio = max(abs(log(aspectRatio1)), abs(log(aspectRatio2)))
  val blankCoverage = split.width.toDouble / (if (split.isVertical) box.width.toDouble else box.height.toDouble)
  val absoluteArea = min(boxChild1.width*boxChild1.height.toDouble, boxChild2.width*boxChild2.height.toDouble)
  val smallestDimension = min(
    min(boxChild1.height, boxChild1.width),
    min(boxChild2.height, boxChild2.width)
  )

  /**
   * Given a feature vector produces a score
   * @return
   */
  def score() : Double = {

    val widthWeight = 1
    val aspectRatioWeight = 0
    val blankCoverageWeight = 0

    if (smallestDimension > 2 && maxLogAspectRatio < 10/*0.47*/) {
      if (splitWidth.toDouble == 0) {
        1
      } else {
        widthWeight * splitWidth.toDouble
      }
    } else {
      0
    }
  }
}
