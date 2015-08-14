package org.allenai.pdfsubfigures.evaluate

import java.io.File
import javax.imageio.ImageIO

import org.allenai.pdfsubfigures.dissect.{BoxWriter, RecursiveDissector, PngDissector}
import org.allenai.pdfsubfigures.geometry.Box
import scala.util.control.Breaks._

/**
 * Created by noahseigel on 8/13/15.
 */
object EvaluateAccuracy {

  def main(args: Array[String]): Unit = {
    

  }

  // True positives, false positives, false negatives
  def evalFoundBoxes(foundBoxes: List[Box], trueBoxes: List[Box]): (Int, Int, Int) = {
    var truePositives = 0
    val intersectionThreshold = .75
    for (foundBox <- foundBoxes){
      breakable {
        for (trueBox <- trueBoxes) {
          if (boxOverlap(foundBox, trueBox) >= intersectionThreshold) {
            truePositives += 1
            break
          }
        }
      }
    }
    val falsePositives = foundBoxes.length-truePositives
    val falseNegatives = trueBoxes.length-truePositives
    (truePositives,falsePositives,falseNegatives)
  }

  def boxOverlap(bb1: Box, bb2: Box): Double = {
    if(bb1.xEnd < bb2.xStart || bb2.xEnd < bb1.xStart || bb1.yEnd < bb2.yStart || bb2.yEnd < bb1.yStart) {
      return 0
    }
    val intersectBox = new Box(Math.max(bb1.xStart,bb2.xStart),Math.max(bb1.yStart,bb2.yStart),Math.min(bb1.xEnd,bb2.xEnd),Math.min(bb1.yEnd,bb2.yEnd));
    val unionArea = area(bb1)+area(bb2)-area(intersectBox)
    area(intersectBox).toDouble/unionArea
  }

  def area(b: Box): Int = {
    (b.xEnd-b.xStart)*(b.yEnd-b.yStart)
  }
}

object Evaluator extends App {

  val perPngEvaluations =
    listPngs(args(0)).flatMap { pngFile =>
      val goldFile = goldFileFor(pngFile)
      if (!goldFile.exists()) {
        None
      } else {
        val predictedBoxes = RecursiveDissector.findBoxes(pngFile.getAbsolutePath)
        val unGoldBoxes = BoxWriter.boxFromAnnotation(goldFile.getAbsolutePath)
        val goldBoxes =
          unGoldBoxes.map(RecursiveDissector.crop(pngFile.getAbsolutePath))
        val (truePos, falsePos, falseNeg) =
          EvaluateAccuracy.evalFoundBoxes(predictedBoxes, goldBoxes.toList)
        val (trueUnPos, falseUnPos, falseUnNeg) =
          EvaluateAccuracy.evalFoundBoxes(predictedBoxes, unGoldBoxes.toList)
        val result =
          s"cropped: ${pngFile.getName}: TP = $truePos, FP = $falsePos, FN = $falseNeg"
        val unResult =
          s"noCropp: ${pngFile.getName}: TP = $trueUnPos, FP = $falseUnPos, FN = $falseUnNeg"
        if (truePos == trueUnPos && falsePos == falseUnPos && falseNeg == falseUnNeg) {
          //print(".")
        } else {
          //println("")
          //println(result)
          println(unResult)
        }
        Some(pngFile.getName, trueUnPos, falseUnPos, falseUnNeg)
      }
    }

  val (ignored, truePos, falsePos, falseNeg) =
    perPngEvaluations.reduce((t1, t2) => (t1._1, t1._2 + t2._2, t1._3 + t2._3, t1._4 + t2._4))

  println(s"TP: $truePos, FP: $falsePos, FN: $falseNeg")

  def goldFileFor(pngFile: File): File = {
    new File(pngFile.getAbsolutePath.replace("png", "png-annotation.json"))
  }
  def listPngs(dir: String): Array[File] = {
    new File(dir).listFiles().filter(f => f.isFile && f.getPath.endsWith("png"))
  }
}

/*
function overlapRatio = findBoxOverlap(bb1,bb2)
x1 = bb1(1);
x2 = bb2(1);
y1 = bb1(2);
y2 = bb2(2);
w1 = bb1(3);
w2 = bb2(3);
h1 = bb1(4);
h2 = bb2(4);

if(x1+w1 < x2 || x2+w2 < x1 || y1+h1 < y2 || y2+h2 < y1)
    overlapRatio = 0;
    return;
end

ic = [max(x1,x2),max(y1,y2),min(x1+w1-1,x2+w2-1),min(y1+h1-1,y2+h2-1)];
intersectionArea = (ic(3)-ic(1)+1)*(ic(4)-ic(2)+1);
unionArea = h1*w1+h2*w2-intersectionArea;
%disp(['intersection area: ' num2str(intersectionArea)]);
%disp(['union area: ' num2str(unionArea)]);
overlapRatio = intersectionArea/unionArea;
 */