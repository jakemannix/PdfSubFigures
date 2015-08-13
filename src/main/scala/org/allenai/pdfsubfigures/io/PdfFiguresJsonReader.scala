package org.allenai.pdfsubfigures.io

import java.awt.Graphics
import java.awt.image.BufferedImage

import spray.json._
import java.nio.file._

import scala.io.Source

class PdfFiguresJsonReader {
  val downloadDir = "/Users/jake/Downloads/"
  val sampleFileName = downloadDir + "multiline/" + "C02-1100-f3-d200.png"

  val img: BufferedImage = javax.imageio.ImageIO.read(new java.io.File(sampleFileName))
  //img.createGraphics().drawImage(img, 0, 0, null)
//  val height = img.getHeight
//  val width = img.getWidth
//  val raster = img.getData
//  val samples = raster.getPixel(width/2, height/2, null.asInstanceOf[Array[Int]])

  def getPixel(x: Int, y: Int) = {
 //   raster.getPixel(x, y, null.asInstanceOf[Array[Int]])
  }
}

object PdfFiguresJsonReader extends App {
  val fileName = args(0)
  println(fileName)
}