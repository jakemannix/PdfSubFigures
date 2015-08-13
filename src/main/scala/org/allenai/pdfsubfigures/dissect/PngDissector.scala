package org.allenai.pdfsubfigures.dissect

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO


class PngDissector(img: BufferedImage) {

  /**
   * Writes a black line horizontally across the image, and returns the modified image
   * @param y height at which to draw the line
   * @param xStart
   * @param xEnd
   * @return
   */
  def addHorizBlackLine(y: Int, xStart: Int = 0, xEnd: Int = img.getWidth - 1) = {
    for (x <- xStart until xEnd) {
      img.setRGB(x, y, 0)
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
  def addVertBlackLine(x: Int, yStart: Int = 0, yEnd: Int = img.getHeight - 1) = {
    for (y <- yStart until yEnd) {
      img.setRGB(x, y, 0)
    }
    img
  }
}

object PngDissector extends App {
  val fileName = args(0)
  val lineY = args(1).toInt
  val outFileName = args(2)
  val pngEditor = new PngDissector(ImageIO.read(new File(fileName)))
  val outImg = pngEditor.addHorizBlackLine(lineY)
  ImageIO.write(outImg, "png", new File(outFileName))
}