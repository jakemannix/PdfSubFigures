package org.allenai.pdfsubfigures.io

import spray.json._
import sys.process._

object getJSONPages {

  def main(args: Array[String]) {


    val prefix= args(0).stripSuffix(".pdf")

    println("Running pdffigures on " + args(0))

    "/Users/roiel/Ai2/hackathon/pdffigures/pdffigures -j " + prefix + " " + args(0) + " " !

    val json = scala.io.Source.fromFile(prefix + ".json").mkString.parseJson.asInstanceOf[JsArray]
    val figList = json.elements.toList.map(_.asJsObject).filter(x => x.fields("Type").toString.trim.equals("\"Figure\""))
    val pageList = figList.map(_.fields("Page")).distinct

    print("Rasterizing pages ")
    pageList.map(x => print(x + " "))
    print("using ghostscript")

//
//    for (element <- figList) {
//      element.fields("Page")
//      element.fields
//
//    }


  }
}
