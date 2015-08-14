package org.allenai.pdfsubfigures.dissect

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{FileWriter, BufferedWriter}

import spray.json._
import org.allenai.pdfsubfigures.geometry.Box

object BoxWriter {
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

  def boxToJson(box: Box, imageName: String, outpath: String) = {
    val jsonFromBox =  box.toJson
    val w = new BufferedWriter(new FileWriter(outpath))
    w.write(jsonFromBox.prettyPrint)
    w.close
  }

  object boxJsonProtocol extends DefaultJsonProtocol {
    implicit val boxFormat = jsonFormat4(Box)
  }

}
