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

  var count = 0
  
  
  /** Classifies a single KaggleData using the training set and k value for this KNN */
  override def classify(d: KaggleData): KaggleLabel.Value = {

    /** Inserts elm into the list at the correct index to maintain acc's sorting.
     *  Assumes sorting is by double field of elements in list (similarity to d)
     *  Deletes the last element if over length k
     *  acc is in form (unprocessed, processed
     */
    def r(acc : (List[(KaggleData, Double)], List[(KaggleData, Double)]), elm : KaggleData) : List[(KaggleData, Double)] = {
      def r(acc : (List[(KaggleData, Double)], List[(KaggleData, Double)]), elm : (KaggleData, Double)) : List[(KaggleData, Double)] = {
        acc match{
          case (List(), processed) => 
            if(processed.length < k) (elm :: processed).reverse
            else processed.reverse
          case (unprocessed, processed) => 
            if(elm._2 > unprocessed.head._2){
              val lst = processed.reverse ::: (elm :: unprocessed)
              if(lst.length > k) lst.dropRight(lst.length - k)
              else lst
            } else{
              r((unprocessed.tail, unprocessed.head :: processed), elm)
            }
        }
      }
      r(acc, (elm, elm.similarity(d)))
    }
    
    //Create list of top k examples
    val lst = training.foldLeft(List[(KaggleData, Double)]())(
      (acc, a) => r( (List[(KaggleData, Double)](), acc), a)    
    ).map(a => a._1)
    
    //Convert to map
    val map = lst.foldLeft(Map[KaggleLabel.Value, Int]())((acc, e) => {
      val v = acc.getOrElse(e.label, 0)
      acc - e.label + ((e.label, v+1))
    })
    
    val l = map.toList.reduceLeft((a, b) => if(a._2 > b._2) a else b)._1
    count = count+1
    //println(count)
    l
  }
}