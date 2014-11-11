package classifier

import scala.collection.immutable.HashMap
import scala.annotation.tailrec
import data._

//TODO - ties?
/** KaggleData Naive Bayes implementation on Message objects, Predicts ties as Y = 1 (True)
 *  Initialize once for each list of messages to predict on
 *  @author MPatashnik
 */
class NaiveBayes(val data : List[KaggleData], val vocabulary : List[String]) {
  
  /** Map of label y -> Map of term index -> frequency. Notably, all frequencies are +1 */
  val termOccuranceMap = data.foldLeft(
       data.head.labels.foldLeft(new HashMap[KaggleLabel.Value, Map[Int, Double]]())(
         (acc, e) => acc + ((e, new HashMap[Int, Double]()))
       ))(termOccuranceFun).map(v => (v._1, v._2.map(t => (t._1, t._2 + 1))))
         
  /** Folder function for creating termOccuranceMap */
  private def termOccuranceFun (acc : HashMap[KaggleLabel.Value, Map[Int, Double]], e : KaggleData) : HashMap[KaggleLabel.Value, Map[Int, Double]] = {
    val m = acc.getOrElse(e.label, new HashMap[Int, Double]())
      
    /** Adds the tuple e to acc. If e's key is already there, combines doubles, otherwise adds as new tuple */
    def termAdderFun (acc : Map[Int, Double], e : (Int, Double)) : Map[Int, Double] = {
      val o = acc.getOrElse(e._1, 0.0)
      acc - e._1 + ((e._1, e._2 + o))
    }
      
    //New map of term frequency after this message's terms are added
    val m2 = e.vals.foldLeft(m)(termAdderFun)
      
    //Remove old key, put this one in
    acc - e.label + ((e.label, m2))
  }
    
  /** Map of label y -> Number of tokens. Notably, all + size of vocabulary */
  val termSizeMap = data.foldLeft(new HashMap[KaggleLabel.Value, Double]()
      )(termSizeFun).map(a => (a._1, a._2 + vocabulary.size))
       
  /** Folder function for creating termSizeMap */
  private def termSizeFun (acc : HashMap[KaggleLabel.Value, Double], e : KaggleData) : HashMap[KaggleLabel.Value, Double] = {
    val m = acc.getOrElse(e.label, 0.0)
      
    //New map of term frequency after this message's terms are added
    val m2 = e.vals.foldLeft(m)((acc, v) => acc + v._2)
      
    //Remove old key, put this one in
    acc - e.label + ((e.label, m2))
  }
  
  /** Map of label y -> number of documents classified as y */
  val countClassMap = Data.labelCount(data).map(a => (a._1, a._2.toDouble))
    
  /** Total count of documents */
  val documentCount = data.length.toDouble
    
  /** Returns p(Y = y) */
  private def pY(y : KaggleLabel.Value) : Double = {
    val i = countClassMap.getOrElse(y, 0.0) / documentCount
    i
  }
  
  /** Returns p(W = w | Y = y) */
  private def pWY(w : Int, y : KaggleLabel.Value) : Double = {
    val n = termOccuranceMap.get(y) match{
      case Some(a) => a.getOrElse(w, 1.0)
      case None => 1.0
    }
    val d = termSizeMap.getOrElse(y, vocabulary.size.toDouble)
    n/d
  }
  
  /** Classifies the given message (not looking at its label.
   *  Only functions properly for data in index -> # of occurances form
   *  For part 3b */
  def classify(message : KaggleData, pos : KaggleLabel.Value, neg : KaggleLabel.Value) : KaggleLabel.Value = {
    classify(message, (1.0, pos), (1.0, neg))
  }
  
  /** Classifies the given message with the given weights c00, c01, c10, c11.
   *  pos : cost of mislabeling a negative example (false positive) and the positive label
   *  neg : cost of mislabelign a positive example (false negative) and the negative label */
  def classify(message : KaggleData, pos : (Double, KaggleLabel.Value), neg : (Double, KaggleLabel.Value)) : KaggleLabel.Value = {
    val p = message.labels.map(
        label => message.vals.foldLeft(Math.log10(pY(label)))(
        		(acc, a) => acc + Math.log10(pWY(a._1, label)) * a._2 
    ))
    
    val probs = message.labels.zip(p).toMap
    
    
    if(probs(pos._2) + Math.log10(pos._1 - 0) >= 
      probs(neg._2) + Math.log10(neg._1 - 0)  ){
      pos._2
    } else{
      neg._2
    }
    
  }
  
  /** Trains the Naive Bayes on the given training set. Returns a tuple of
   *  (false positives, false negatives)
   */
  def train(t : List[KaggleData], pos : KaggleLabel.Value, neg : KaggleLabel.Value) : (Int, Int) = {
    train(t, (1.0, pos), (1.0, neg))
  }
  
  /** Trains the Naive Bayes on the given training set. 
   *  Input after train list is (penalty for false positive, positive label),
   *  (penalty for false neg, neg label)
   *  
   *  Returns a tuple of
   *  (false positives, false negatives)
   */
  def train(train : List[KaggleData], pos : (Double, KaggleLabel.Value), neg : (Double, KaggleLabel.Value)) : (Int, Int) = {
    def f(acc : (Int, Int), m : KaggleData) : (Int, Int) = {
      val l = classify(m, pos, neg)
      if(l.equals(m.label)) acc
      else{
        if(l.equals(pos._2)) (acc._1 + 1, acc._2)
        else (acc._1, acc._2 + 1)
      }
    }
    train.foldLeft((0,0))(f)
  }
  
  //TODO - fix issues if we want to know key-attributes
//    /** The n most helpful-strongest HAM words and the n most helpful-strongest SPAM
//   *  words. In form (ham words in descending helpfulness
//   *  for part 3c
//   */
//   def calcKeywords(n : Int) : (List[(Int, Double)], List[(Int, Double)]) = {
//     val a = (0 until vocabulary.size) //Possible word indices that could win
//     val l : (List[(Int, Double)], List[(Int, Double)]) = (List(), List())
//     
//     def f(acc : (List[(Int, Double)], List[(Int, Double)]), e : Int) : (List[(Int, Double)], List[(Int, Double)]) = {
//       //Calculate probabilities for new word
//       val t = (e, Math.log(pWY(e, MessageLabel.HAM)) - Math.log(pWY(e, MessageLabel.SPAM))) //Log probs
//       
//       //Do recursion to see if it should be inserted into list
//       //acc is (unprocessed, processed).
//       //Comp is (old elm, new elm) => new elm is relevant
//       @tailrec
//       def r(acc : (List[(Int, Double)], List[(Int, Double)]), elm : (Int, Double), 
//           comp : ((Int, Double), (Int, Double)) => Boolean) : List[(Int, Double)] = {
//         acc match{
//           case (List(), p) => 
//             if(p.length < n) (elm :: p).reverse
//             else p.reverse
//           case (u, p) => 
//             if(comp(u.head, elm)){
//               val lst = p.reverse ::: (elm :: u)
//               if(lst.length > n) lst.dropRight(lst.length - n)
//               else lst
//             } else{
//               r((u.tail, u.head :: p), elm, comp)
//             }
//         }
//       }
//       (r( (acc._1, List()), t, (a : (Int, Double), b : (Int, Double)) => b._2 > a._2),
//        r( (acc._2, List()), t, (a : (Int, Double), b : (Int, Double)) => b._2 < a._2))
//     }
//     
//     a.foldLeft(l)(f)
//  }
}