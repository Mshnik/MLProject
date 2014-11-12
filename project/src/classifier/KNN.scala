package classifier

import io._
import data._

/** Assistant class for KNN implementation. Also holds the main method for running KNN */
object KNN {

  /** Runner for KNN */
  def main(args : Array[String]) : Unit = {
     trainTest(4,5,3)
  }
  
  /** Basic run - train on combined_train, test on combined_test, with max depth d */
  def trainTest(train : Int, test : Int, k : Int) : Unit = {
    val trainList = ReaderWriter.readSVMData(ReaderWriter.svmScaledFile(train), KaggleLabel.stringToLabelMap, 0)
    println("Read data from " + train)
    val knn = new KNN(trainList, k)
    val a = knn.test(ReaderWriter.readSVMData(ReaderWriter.svmScaledFile(test), KaggleLabel.stringToLabelMap, 0))
    println("Tested on " + test + " : " + a + " accuracy = " + ((a._1 + a._4).toDouble/(a._1 + a._2 + a._3 + a._4).toDouble) + "\n")
  }
}

/** Creates a KNN classifier with the given training data and k value.
 *  Make sure the input set is normalized or some attributes will get more attention. */
class KNN(val training: List[KaggleData], val k: Int) extends AbsClassifier {

  /** Classifies a single KaggleData using the training set and k value for this KNN */
  override def classify(d: KaggleData): KaggleLabel.Value = {
    
    /** Creates a priority queue of the k closest examples to d */
    val kClosest = new java.util.PriorityQueue[KaggleData](1, new java.util.Comparator[KaggleData]() {
      override def compare(o1: KaggleData, o2: KaggleData): Int = {
        var diff = (d.similarity(o2) - d.similarity(o1))
        if (diff < 0) return -1
        if (diff > 0) return 1
        return 0
      }
    })
    
    // Add all elements to priority queue
    for (p2 <- training) {
      kClosest.add(p2)
    }
    //For the first k elements, copy to list.
    val lst = (0 until k).toList.foldLeft(List[KaggleData]())((acc, e) => kClosest.poll() :: acc).reverse
        
    //Convert to map
    val map = lst.foldLeft(Map[KaggleLabel.Value, Int]())((acc, e) => {
      val v = acc.getOrElse(e.label, 0)
      acc - e.label + ((e.label, v+1))
    })
    
    map.toList.reduceLeft((a, b) => if(a._2 > b._2) a else b)._1
  }
}