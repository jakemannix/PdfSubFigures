package org.allenai.pdfsubfigures.dissect

import org.allenai.pdfsubfigures.geometry.{Box, Split}

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

  val width = split.width.toDouble
  val aspectRatio = math.max(math.abs(math.log(boxChild1.height.toDouble/boxChild1.width.toDouble)),
    math.abs(math.log(boxChild2.height.toDouble/boxChild2.width.toDouble)))
  val blankCoverage = split.width.toDouble / (if (split.isVertical) box.width.toDouble else box.height.toDouble)
  val absoluteArea = math.min(boxChild1.width*boxChild1.height.toDouble, boxChild2.width*boxChild2.height.toDouble)
  val smallestDimension = math.min(math.min(boxChild1.height, boxChild1.width),math.min(boxChild2.height, boxChild2.width))

  /**
   * Given a feature vector produces a score
   * @return
   */
  def score() : Double = {

    val widthWeight = 1
    val aspectRatioWeight = 0
    val blankCoverageWeight = 0

    return if (smallestDimension > 20 && aspectRatio < 0.47) widthWeight*width.toDouble else 0
  }
}
