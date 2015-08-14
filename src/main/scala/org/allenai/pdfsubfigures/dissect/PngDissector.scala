package org.allenai.pdfsubfigures.dissect

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import org.allenai.pdfsubfigures.geometry._

import scala.collection.immutable.IndexedSeq


class PngDissector(val img: BufferedImage) {
  /**
   * Writes a  line horizontally across the image, and returns the modified image
   * @param y height at which to draw the line
   * @param xStart
   * @param xEnd
   * @param rgb color
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
      yEnd: Int = img.getHeight - 1): List[Int] = {
    val cols = (xStart until xEnd).filter(x => (yStart until yEnd).forall(y =>
      isWhite(new Color(img.getRGB(x, y))))).toList
    cols
  }

  def findWhiteRows(xStart: Int = 0,
      yStart: Int = 0,
      xEnd: Int = img.getWidth - 1,
      yEnd: Int = img.getHeight - 1): List[Int] =
    (yStart until yEnd).filter(y => (xStart until xEnd).forall(x =>
      isWhite(new Color(img.getRGB(x, y))))).toList


  def findWhiteColumns(box: Box): List[Int] = {
    findWhiteColumns(xStart = box.xStart, xEnd = box.xEnd, yStart = box.yStart, yEnd = box.yEnd)
  }

  def findWhiteRows(box: Box): List[Int] = {
    findWhiteRows(xStart = box.xStart, xEnd = box.xEnd, yStart = box.yStart, yEnd = box.yEnd)
  }

  def findWhiteColumnSplits(box: Box): List[Split] = {
    val splits = PngDissector.rowsToSplits(findWhiteColumns(box).toArray, isVertical = true)
    splits
  }

  def findWhiteRowSplits(box: Box): List[Split] = {
    val splits = PngDissector.rowsToSplits(findWhiteRows(box).toArray, isVertical = false)
    splits
  }

  def findSplitGuesses(box: Box): List[Split] = {
    val colSplits = findWhiteColumnSplits(box)
    val rowSplits = findWhiteRowSplits(box)
    val guesses = colSplits ++ rowSplits
    guesses
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

  def rowsToSplits(ints: Array[Int], isVertical: Boolean): List[Split] = {
    val pairs =
      ints.zipWithIndex.map { case(v,i) => if (i==0) { (v,v) } else { (v, v - ints(i-1) - 1)} }
    val flattened = pairs.flatMap {
      case ((x, 0)) => None
      case ((x, offset)) => Some((x, offset))
    }
    val magic = flattened.flatMap(p => List(p._1 - p._2 - 1, p._1))
    val res = (List(ints(0)) ++ magic.toList ++ List(ints(ints.length - 1))).grouped(2).map(l =>
      Split(l(0), l(1), isVertical)
    ).toList
    res.filterNot(s => s.start < 0 || s.end < 0)
  }

}

object PngDissectorApp extends App {
  args.foreach { fileName =>
    val outFileName = fileName.replace("png", "redlines.png")
    val outJsonFile = fileName.replace("png", "boxes.json")
    val pngDissector = new PngDissector(ImageIO.read(new File(fileName)))
    val img = pngDissector.img
    val otherDissector = new RecursiveDissector(pngDissector.img)
    val startBox = Box(xStart = 0, yStart = 0,
      xEnd = img.getWidth, yEnd = img.getHeight)
    val outBoxes = otherDissector.split(startBox)
    val redImg = BoxWriter.writeBoxes(img, outBoxes)
    BoxWriter.writeAllBoxes(outBoxes, outJsonFile)
    val inBoxes = BoxWriter.readAllBoxes(outJsonFile)
    ImageIO.write(redImg, "png", new File(outFileName))
  }
}

