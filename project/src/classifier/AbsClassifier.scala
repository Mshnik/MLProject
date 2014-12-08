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
  
  /** Trains, validates, and tests. Whoo!
   *  Criteria - a string describing what the evaluation criteria is
   *  eval - a function that takes (TP, FN, FP, TN) and returns a double of how good this is
   *  trainList - a list of kaggleData to train on
   *  validateList - a list of kaggleData to validate on
   *  testList - a list of kaggleData to test on.
   *  trainer - a function that given an arg and a list of data returns a classifier
   *  argsList - a list of args to try the trainer on.  */
  def trainValidateTest[T](criteria : String, eval : ((Int, Int, Int, Int)) => Double,
		  				trainList : List[KaggleData], validateList : List[KaggleData],
		  				testList : List[KaggleData], 
		  				trainer : (List[KaggleData], T) => AbsClassifier,
		  				argsList : List[T]) : Unit = {
    
    // Create validate and testing functions
    def validate = check(validateList, "Validate", false)_
    def test = check(testList, "Test", true)_
    
    
    var best : (AbsClassifier, (Int, Int, Int, Int)) = null
    
    for(arg <- argsList){
      val n : AbsClassifier = trainer(trainList, arg)
      val v = validate(n)
      if(best == null || eval(best._2) < eval(v)){
        best = (n, v)
      }
    }
    val depth = 2
    
    System.out.println("Best " + criteria + ": " + best._1.description + " (TP, FN, FP, TN)" + best._2)
    System.out.println(best._1.toString)
    test(best._1)
    System.out.println("--------------------------------------------")
    System.out.println("--------------------------------------------")
  }
  
  /** validates or tests the classifier on the given test set. Returns tuple of (TP, FN, FP, TN) */
  private def check(lst : List[KaggleData], msg : String, print : Boolean)(classifier : AbsClassifier) : (Int, Int, Int, Int) = {
    val a = classifier.test(lst)
    if(print){
	    System.out.println(msg + " : " + a)
	    System.out.println("  Accuracy = " + AbsClassifier.accuracy(a))
	    System.out.println("  Recall = " + AbsClassifier.recall(a))
	    System.out.println("  Precision = " + AbsClassifier.precision(a))
	    val f1 = AbsClassifier.fOne(a._1, a._2, a._3, a._4)
	    System.out.println("  F1 = " + f1)
	    System.out.println("  F0.5 = " + AbsClassifier.f(0.5)(a._1, a._2, a._3, a._4))
    }
    a
  }
}

/** Parent of all classifiers.
 *  Signifies the ability to test a list of data.
 */
abstract class AbsClassifier(val description : String) {
  
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