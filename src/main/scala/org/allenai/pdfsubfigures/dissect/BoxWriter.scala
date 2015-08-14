package org.allenai.pdfsubfigures.dissect

import java.awt.Color
import java.awt.image.BufferedImage

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
}
