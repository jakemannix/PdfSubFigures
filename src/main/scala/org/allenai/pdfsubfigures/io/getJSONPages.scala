package org.allenai.pdfsubfigures.io

import java.awt.image.BufferedImage
import java.io.{FileWriter, BufferedWriter, File}
import javax.imageio.ImageIO

import spray.json._
import sys.process._
import scala.language.postfixOps
import scala.io.Source


object getJSONPages {

  /**
   * Script that extracts figures from pdf files
   * Outputs .pngs to a .png
   * @param args args(0) is the directory of the .pdf files to be converted
   *             args(1) is the path to the pdffigures executable
   */
  def main(args: Array[String]) {

    val pathFile = new File(args(0))
    var path = pathFile.getAbsolutePath
    if (!path.charAt(path.length-1).equals('/')) path = path + '/'

    val jsonDir = new File (path + "/JSON/")
    if (!jsonDir.exists())
      jsonDir.mkdir()

    val pngDir = new File (path + "/PNG/")
    if (!pngDir.exists())
      pngDir.mkdir()

    for (file <- pathFile.listFiles().filter(_.toString.endsWith(".pdf"))) {

      val pdf = file.getAbsolutePath
      val prefix = file.getName.stripSuffix(".pdf")

      println("Running pdffigures on " + path + prefix)

      args(1) + " -j " + path + prefix + " " + pdf !

      val json = scala.io.Source.fromFile(path + prefix + ".json").mkString.parseJson.asInstanceOf[JsArray]
      val figList = json.elements.toList.map(_.asJsObject).filter(x => x.fields("Type").toString.trim.equals("\"Figure\""))
      val pageList = figList.map(_.fields("Page").toString.toInt)
      val pageSet = pageList.distinct

      println("Rasterizing pages with GS")

      for (page <- pageSet) {
        val gsArgs = "-dSAFER -dBATCH -dNOPAUSE -sDEVICE=png16m -dFirstPage=" + page + " -dLastPage=" + page +
          " -r200 -dTextAlphaBits=1 -dGraphicsAlphaBits=1 -sOutputFile=" + path + prefix + "_" + page + ".png " + pdf
        "gs " + gsArgs !
      }

      val imageFiles = pageSet.map(x => new File(path + prefix + "_" + x + ".png"))
      val images = pageSet.zip(imageFiles.map(x => ImageIO.read(x))).toMap[Int, BufferedImage]

      val bbList = figList.map(x => x.fields("ImageBB").asInstanceOf[JsArray].elements.map(_.toString().toInt))
      
      for (i <- 0 until figList.length) {
        val croppedFig = images(pageList(i)).getSubimage(bbList(i)(0) * 2,
          bbList(i)(1) * 2,
          (bbList(i)(2) - bbList(i)(0)) * 2,
          (bbList(i)(3) - bbList(i)(1)) * 2)

        ImageIO.write(croppedFig, "png", new File(pngDir.getAbsolutePath + "/" + prefix + "-fig" + i + ".png"))

        val w = new BufferedWriter(new FileWriter(jsonDir.getAbsolutePath + "/" + prefix + "-fig" + i + ".json"))
        w.write(figList(i).prettyPrint)
        w.close

      }
      val junkJSON = new File(path + prefix + ".json")
      print("Junk: " + junkJSON)
      junkJSON.delete()
      imageFiles.map(_.delete())
    }
  }
}
