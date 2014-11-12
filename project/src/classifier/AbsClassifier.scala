package classifier

import data._

/** Parent of all classifiers.
 *  Signifies the ability to test a list of data.
 */
abstract class AbsClassifier {

   /** Classifies each element in the input set, outputs (TP, FN, FP, TN). */
  @throws[RuntimeException]
  def test(testList : List[KaggleData]) : (Int, Int, Int, Int) = {
    def f(acc : (Int, Int, Int, Int), e : KaggleData) : (Int, Int, Int, Int) = {
      val c = classify(e)
      (e.label, c) match{
        case (KaggleLabel.TRUE, KaggleLabel.TRUE) => (acc._1 + 1, acc._2, acc._3, acc._4)
        case (KaggleLabel.TRUE, KaggleLabel.FALSE) => (acc._1, acc._2 + 1, acc._3, acc._4)
        case (KaggleLabel.FALSE, KaggleLabel.TRUE) => (acc._1, acc._2, acc._3 + 1, acc._4)
        case (KaggleLabel.FALSE, KaggleLabel.FALSE) => (acc._1, acc._2, acc._3, acc._4 + 1)
        case _ => throw new RuntimeException("Strange classification for " + e + " : " + c)
      }
    }
    testList.foldLeft((0,0,0,0))(f)
  }
  
  /** Classifies the given data using this classifier */
  def classify(e : KaggleData) : KaggleLabel.Value
  
}