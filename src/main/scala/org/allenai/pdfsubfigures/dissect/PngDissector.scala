package org.allenai.pdfsubfigures.dissect

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.collection.immutable.IndexedSeq


class PngDissector(val img: BufferedImage) {
  /**
   * Writes a  line horizontally across the image, and returns the modified image
   * @param y height at which to draw the line
   * @param xStart
   * @param xEnd
   * @rgb color
   * @return
   */
  def addHorizLine(y: Int, xStart: Int = 0, xEnd: Int = img.getWidth - 1,
      rgb: Color = Color.BLACK) = {
    for (x <- xStart until xEnd) {
      img.setRGB(x, y,
        (rgb.getAlpha() << 24) | (rgb.getRed() << 16) | (rgb.getGreen() << 8) | (rgb.getBlue() << 0))
    }
    img
  }

  /**
   * Writes a black line vertically down the image, and returns the modified image
   * @param x position at which to draw the line
   * @param yStart
   * @param yEnd
   * @return
   */
  def addVertLine(x: Int, yStart: Int = 0, yEnd: Int = img.getHeight - 1,
      rgb: Color = Color.BLACK) = {
    for (y <- yStart until yEnd) {
      img.setRGB(x, y,
        (rgb.getAlpha() << 24) | (rgb.getRed() << 16) | (rgb.getGreen() << 8) | (rgb.getBlue() << 0))
    }
    img
  }

  def toMatrix(xStart: Int = 0,
      yStart: Int = 0,
      xEnd: Int = img.getWidth - 1,
      yEnd: Int = img.getHeight - 1): IndexedSeq[(Int, Int, Color)] = {
    for { x <- xStart until xEnd
          y <- yStart until yEnd } yield {
      (x, y, new Color(img.getRGB(x, y)))
    }
  }

  def findWhiteColumns(xStart: Int = 0,
      yStart: Int = 0,
      xEnd: Int = img.getWidth - 1,
      yEnd: Int = img.getHeight - 1): List[Int] =
    (xStart until xEnd).filter(x => (yStart until yEnd).forall(y =>
      isWhite(new Color(img.getRGB(x, y))))).toList

  def findWhiteRows(xStart: Int = 0,
      yStart: Int = 0,
      xEnd: Int = img.getWidth - 1,
      yEnd: Int = img.getHeight - 1): List[Int] =
    (yStart until yEnd).filter(y => (xStart until xEnd).forall(x =>
      isWhite(new Color(img.getRGB(x, y))))).toList

  def findWhiteColumnEdges(xStart: Int = 0,
      yStart: Int = 0,
      xEnd: Int = img.getWidth - 1,
      yEnd: Int = img.getHeight - 1): List[Int] = {
    ???
  }

  def bestSplit(splits: List[(Int, Int)]) : Option[(Int, Int)] = {

    val features = splits.map(x => getFeatures(x))
    val score = features.map(x => x.score())
    val threshold = 5
    val candidate = score.reduceLeft((x, y) => if (x > y) x else y)
    if (candidate > threshold) Some(splits(score.indexOf(candidate))) else None
  }

  /**
   * @param split
   * @return a list of relevant features
   */
  def getFeatures(split: (Int, Int)) : FeatureVector = {

    ???
  }



  def reddenWhiteColumns(xStart: Int = 0,
      yStart: Int = 0, xEnd: Int = img.getWidth - 1, yEnd: Int = img.getHeight - 1) = {
    findWhiteColumns(xStart, yStart, xEnd, yEnd).foreach { x =>
      addVertLine(x, yStart, yEnd, Color.RED)
    }
    new PngDissector(img)
  }

  def reddenWhiteRows(xStart: Int = 0,
      yStart: Int = 0, xEnd: Int = img.getWidth - 1, yEnd: Int = img.getHeight - 1) = {
    findWhiteRows(xStart, yStart, xEnd, yEnd).foreach { y =>
      addHorizLine(y, xStart, xEnd, Color.RED)
    }
    new PngDissector(img)
  }

  def write(outFileName: String): Unit = {
    ImageIO.write(img, "png", new File(outFileName))
  }

  def isWhite(rgba: Color, ignoreRed: Boolean = true): Boolean = {
    //(ignoreRed || asColor.getRed == 255) &&
    rgba == Color.RED ||
        (rgba.getGreen == 255 && rgba.getBlue == 255)
  }
}

object PngDissector {
  def apply(fileName: String) = new PngDissector(ImageIO.read(new File(fileName)))
}

object PngDissectorApp extends App {
  val fileName = args(0)
  val outFileName = fileName.replace("png", "redlines.png")
  val pngDissector = new PngDissector(ImageIO.read(new File(fileName)))
  val whiteColumns = pngDissector.findWhiteColumns()
  val whiteRows = pngDissector.findWhiteRows()
  val outImg = pngDissector.reddenWhiteColumns().reddenWhiteRows()
  ImageIO.write(outImg.img, "png", new File(outFileName))
}