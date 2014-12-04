package classifier

import data._

/** Companion object for All classifiers.
 *  Holds statistical testing methods
 */
object AbsClassifier{
  
  /** Returns the accuracy of the given set of outputs */
  def accuracy(tp : Int, fn : Int, fp : Int, tn : Int) : Double = {
    (tp + tn).toDouble / (tp + fn + fp + tn).toDouble
  }
  
  /** See accuracy(int, int, int, int) */
  def accuracy(a : (Int, Int, Int, Int)) : Double = accuracy(a._1, a._2, a._3, a._4)
  
  /** Returns the recall of the given set of outputs */
  def recall(tp : Int, fn : Int, fp : Int, tn : Int) : Double = {
    tp.toDouble / (tp + fn).toDouble
  }
  
  /** See recall(int, int, int, int) */
  def recall(a : (Int, Int, Int, Int)) : Double = recall(a._1, a._2, a._3, a._4)
  
  /** Returns the precision of the given set of outputs */
  def precision(tp : Int, fn : Int, fp : Int, tn : Int) : Double = {
    tp.toDouble / (tp + fp).toDouble
  }
  
  /** See precision(int, int, int, int) */
  def precision(a : (Int, Int, Int, Int)) : Double = precision(a._1, a._2, a._3, a._4)
  
  /** Returns the value of the f score for the given beta. Beta = 1 is the fOne score */
  def f(beta : Double)(tp : Int, fn : Int, fp : Int, tn : Int) : Double = {
    val betaSq = beta * beta
    val prec = precision(tp, fn, fp, tn)
    val rec = recall(tp, fn, fp, tn)
    (1.0 + betaSq) * (prec * rec) / (betaSq * prec + rec)
  }
  
  /** Returns the value of the fOne score, the harmonic mean of precision and recall */
  def fOne = f(1.0)_
}

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
        case (KaggleLabel.RURAL, KaggleLabel.RURAL) => (acc._1 + 1, acc._2, acc._3, acc._4)
        case (KaggleLabel.RURAL, _) => (acc._1, acc._2 + 1, acc._3, acc._4)
        case (_, KaggleLabel.RURAL) => (acc._1, acc._2, acc._3 + 1, acc._4)
        case (_, _) => (acc._1, acc._2, acc._3, acc._4 + 1)
        case _ => throw new RuntimeException("Strange classification for " + e + " : " + c)
      }
    }
    testList.foldLeft((0,0,0,0))(f)
  }
  
  /** Classifies the given data using this classifier */
  def classify(e : KaggleData) : KaggleLabel.Value
  
}