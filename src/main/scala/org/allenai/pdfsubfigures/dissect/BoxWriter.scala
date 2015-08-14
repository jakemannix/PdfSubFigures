package org.allenai.pdfsubfigures.dissect

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{PrintWriter, File, FileWriter, BufferedWriter}
import javax.imageio.ImageIO

import spray.json._
import org.allenai.pdfsubfigures.geometry.Box

import scala.io.Source

object BoxWriter extends DefaultJsonProtocol {


  def drawBox(img: BufferedImage, box: Box, color: Color): BufferedImage = {
    def setRed(x: Int, y: Int) {
      img.setRGB(x, y, color.getRGB)
    }
    (box.xStart until box.xEnd).foreach { x =>
      setRed(x, box.yStart)
      setRed(x, box.yEnd - 1)
    }
    (box.yStart until box.yEnd).foreach { y =>
      setRed(box.xStart, y)
      setRed(box.xEnd - 1, y)
    }
    img
  }

  def drawBox(img: BufferedImage, box: Box): BufferedImage = {
    drawBox(img, box, Color.RED)
  }

  def writeBoxes(img: BufferedImage, boxes: List[Box]): BufferedImage = {
    boxes.foreach(box => drawBox(img, box))
    img
  }

  def boxToJson(box: Box, outpath: String) = {
    implicit val boxFormat = jsonFormat(Box.apply, "xStart", "yStart", "xEnd", "yEnd")
    val jsonFromBox =  box.toJson
    val w = new PrintWriter(outpath)
    w.write(jsonFromBox.prettyPrint)
    w.close
  }

  def writeAllBoxes(boxes: List[Box], outPath: String) = {
    implicit val boxFormat = jsonFormat(Box.apply, "xStart", "yStart", "xEnd", "yEnd")
    val w = new PrintWriter(outPath)
    val s = boxes.map(_.toJson.prettyPrint).mkString("[", ", ", "]")
    w.println(s)
    w.close()
  }

  def readAllBoxes(inPath: String): List[Box] = {
    implicit val boxFormat = jsonFormat(Box.apply, "xStart", "yStart", "xEnd", "yEnd")
    Source.fromFile(inPath).mkString.parseJson.convertTo[Array[Box]].toList
  }

  def boxFromAnnotation(inpath: String) : List[Box] = {
    val annotations = scala.io.Source.fromFile(inpath).mkString.parseJson.asJsObject.fields("annotations").asInstanceOf[JsArray].elements
    (for (annotation <- annotations) yield {
      val boxJson = annotation.asJsObject.fields("bounds").asJsObject.fields("coords").asInstanceOf[JsArray]

      println(boxJson.elements(0).asJsObject.fields("x").toString.toDouble.floor.toInt)

      Box(boxJson.elements(0).asJsObject.fields("x").toString.toDouble.floor.toInt,
        boxJson.elements(0).asJsObject.fields("y").toString.toDouble.floor.toInt,
        boxJson.elements(1).asJsObject.fields("x").toString.toDouble.ceil.toInt,
        boxJson.elements(1).asJsObject.fields("y").toString.toDouble.ceil.toInt)
    }).toList
  }

  def visualizeResults (img : BufferedImage, annotation: List[Box], result: List[Box], outpath: String): Unit = {

    var output = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_INT_RGB)
    for(a <- annotation) {
      output = drawBox(img, a, Color.BLUE)
    }
    for (r <- result) {
      output = drawBox(img, r, Color.RED)
    }
    ImageIO.write(output, "png", new File(outpath))
  }



}
