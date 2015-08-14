package org.allenai.pdfsubfigures.dissect

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{File, FileWriter, BufferedWriter}

import spray.json._
import org.allenai.pdfsubfigures.geometry.Box

object BoxWriter extends DefaultJsonProtocol {

  def drawBox(img: BufferedImage, box: Box): BufferedImage = {

    (box.xStart until box.xEnd).foreach { x =>
      img.setRGB(x, box.yStart, Color.RED.getRGB)
      img.setRGB(x, box.yEnd, Color.RED.getRGB)
    }
    (box.yStart until box.yEnd).foreach { y =>
      img.setRGB(box.xStart, y, Color.RED.getRGB)
      img.setRGB(box.xEnd, y, Color.RED.getRGB)
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
    val w = new BufferedWriter(new FileWriter(outpath))
    w.write(jsonFromBox.prettyPrint)
    w.close
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
