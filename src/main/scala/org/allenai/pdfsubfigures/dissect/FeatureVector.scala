package org.allenai.pdfsubfigures.dissect

import java.awt.image.BufferedImage

import org.allenai.pdfsubfigures.geometry.{Box, Split}

class FeatureVector(val split: Split, val box: Box) {


  val boxChild1 = if(split.isVertical) {Box(box.xStart, split.start, box.yStart, box.yEnd) }
                  else {Box(box.xStart, box.xEnd, box.yStart, split.start)}
  val boxChild2 = if(split.isVertical) {Box(split.end, box.xEnd, box.yStart, box.yEnd) }
                  else {Box(box.xStart, box.xEnd, split.end, box.yEnd)}

  val width = split.width
  val aspectRatio = boxChild1.height/boxChild1.width
  val blankCoverage = split.width * (if (split.isVertical) box.height else box.width)
  val absoluteArea = math.min(boxChild1.width*boxChild1.height, boxChild2.width*boxChild2.height)
  val smallestDimension = math.min(math.min(boxChild1.height, boxChild1.width),math.min(boxChild2.height, boxChild2.width))

  /**
   * Given a feature vector produces a score
   * @return
   */
  def score() : Double = {

    val widthWeight = 1
    val aspectRatioWeight = 0
    val blankCoverageWeight = 0

    return (if (smallestDimension > 20) 1 else 0)*
      widthWeight*width
  }
}
