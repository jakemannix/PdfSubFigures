package org.allenai.pdfsubfigures.dissect

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{PrintWriter, File, FileWriter, BufferedWriter}

import spray.json._
import org.allenai.pdfsubfigures.geometry.Box

import scala.io.Source

object BoxWriter extends DefaultJsonProtocol {

  def drawBox(img: BufferedImage, box: Box): BufferedImage = {
    def setRed(x: Int, y: Int) {
      img.setRGB(x, y, Color.RED.getRGB)
    }
    (box.xStart until box.xEnd).foreach { x =>
      setRed(x, box.yStart + 1)
      setRed(x, box.yEnd - 1)
    }
    (box.yStart until box.yEnd).foreach { y =>
      setRed(box.xStart + 1, y)
      setRed(box.xEnd - 1, y)
    }
    img
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

  def readAllBoxes(inPath: String): Array[Box] = {
    implicit val boxFormat = jsonFormat(Box.apply, "xStart", "yStart", "xEnd", "yEnd")
    Source.fromFile(inPath).mkString.parseJson.convertTo[Array[Box]]
  }

  def boxFromAnnotation(inpath: String) : Seq[Box] = {
    val annotations = scala.io.Source.fromFile(inpath).mkString.parseJson.asJsObject.fields("annotations").asInstanceOf[JsArray].elements
    for (annotation <- annotations) yield {
      val boxJson = annotation.asJsObject.fields("bounds").asJsObject.fields("coords").asInstanceOf[JsArray]

      println(boxJson.elements(0).asJsObject.fields("x").toString.toDouble.floor.toInt)

      Box(boxJson.elements(0).asJsObject.fields("x").toString.toDouble.floor.toInt,
        boxJson.elements(0).asJsObject.fields("y").toString.toDouble.floor.toInt,
        boxJson.elements(1).asJsObject.fields("x").toString.toDouble.ceil.toInt,
        boxJson.elements(1).asJsObject.fields("y").toString.toDouble.ceil.toInt)
    }
  }


}
