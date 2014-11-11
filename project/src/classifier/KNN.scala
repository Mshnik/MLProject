package classifier

import data._

/** Assistant class for KNN implementation. Also holds the main method for running KNN */
object KNN {

}

/** Creates a KNN classifier with the given training data and k value.
 *  Make sure the input set is normalized or some attributes will get more attention. */
class KNN(val training: List[KaggleData], val k: Int) {

  /** Classifies a single KaggleData using the training set and k value for this KNN */
  def classifyK(d: KaggleData): KaggleLabel.Value = {
    
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