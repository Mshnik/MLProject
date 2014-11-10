package classifier

import scala.collection.mutable.HashMap
import data._
import data.Data._

/** Holder for decision tree algorithms */
object DTNode{
  /** The ID3 DT creation algorithm. Returns the node that represents the root of the tree.
   *  Restricts to the given depth. CurrentDepth should be 1 when first passed in */
  def id3[T <: Label#Value, E <: Data[T]](elms : List[E], maxDepth : Int, currentDepth : Int) : DTNode[T, E] = {
    //First base case - no elms. Returns null
    if (elms.isEmpty) return null
    
    //Check for base case - if all elms share a classification, create and return leaf node
    val labels = elms.head.labels //Possible labelings of elements in elms
    for(l <- labels)
      if (elms.forall(a =>a.label.equals(l)))
    	return new DTLeafNode(l, elms)
    
    //Find most common label among elements - break ties with earlier occuring enum
    val labelCount = Data.labelCount(elms).toList
    var m = -1
    var k = -1
    for(i <- 0 until labelCount.length){
      if(m > labelCount(i)._2){
        m = labelCount(i)._2
        k = i
      }
    }
    if(currentDepth == maxDepth){ //Can't go any further - make the best leaf we can
      return new DTLeafNode(labels(k), elms)
    }
    
    //Non-base case. Try to gain some information by splitting elms
    //Splitting criteria: attribute i (1..9) <= j (0 .. 9)
    //Keep track of ideal info gain thus far
    val currentEntropy = entropy(Data.splitByLabel(elms))
    var bestInfoGain : Double = 0
    var bestCriteria : AttributeFunction[E, T] = null
    
    
    //TODO! Figure out criteria for i, j
    for(i <- 0 to 8; j <- 0 to 9){
      //Construct the new criteria for given values of i, j
      val criteria = new AttributeFunction(elms.head, i, (a : Double, b : List[Double]) => a <= b, List(j), ("data(" + i + ")<=" + j))
      //Do the partition based on this criteria
      val lstOne = elms.filter(criteria.fun)
      val lstTwo = elms.filter(criteria.fun.andThen(a => !a))
      
      //Calculate the information gain
      val entropyOne = weightedEntropy(lstOne, elms.length)
      val entropyTwo = weightedEntropy(lstTwo, elms.length)
      val infoGain = currentEntropy - (entropyOne + entropyTwo)
      
      //Check if better than previous best
      if(infoGain > bestInfoGain){
        bestInfoGain = infoGain
        bestCriteria = criteria
      }
    }
    
    //If there is some best criteria to split on, split and recurse.
    //Otherwise, no possible fix - use an impure leaf
    if(bestCriteria != null){
      //Recurse on the two children
      val cOne = id3(elms.filter(bestCriteria.fun), maxDepth, (currentDepth + 1))
      val cTwo = id3(elms.filter(bestCriteria.fun.andThen(a => !a)), maxDepth, (currentDepth + 1))
      
      //Create and return this attribute node for the given criteria
      val m = new HashMap[DTNode[T], AttributeFunction[T]]()
      m += ((cOne, bestCriteria))
      val s2 = bestCriteria.toString.replaceAll("<=", ">")
      m += ((cTwo, new AttributeFunction(bestCriteria, s2)))
      return new DTAttributeNode(m, labels(k), elms)
    } else{
      
      return new DTLeafNode(labels(k), elms)
    }
  }
  
  /** Returns the proportional entropy by calculating the entropy and multiplying by the
   *  fraction of the set this set makes up
   */
  def weightedEntropy[T <: Label#Value](lst : List[Labelable[T]], s : Int) : Double = {
    (lst.length.toDouble / s.toDouble) * entropy(Data.splitByLabel(lst))
  }
  
  /** Returns Log_2(a) */
  def log2(d : Double) : Double = {
    Math.log(d)/Math.log(2)
  }
  
  /** Returns the entropy of the given list partition.
   *  Input is a list of lists - each inner list is a partition of the greater list
   */
  def entropy[T <: Label#Value](mp : Map[T, List[Labelable[T]]]) : Double = {
    val totalSize = mp.foldLeft(0.0)( (a : Double, b : (T,List[Labelable[T]])) => a + b._2.length)
    
    def f (acc : Double, b : (T, List[Labelable[T]])) = {
     val c = if(b._2.length == 0) 0 else
       (b._2.length.toDouble/totalSize)*log2((b._2.length.toDouble/totalSize))
     acc - c
    }
    
    mp.foldLeft(0.0)(f)
  }
}

/** Abstract node class that represents a Decision tree.
 *  May be asked to classify a given data to a label (T),
 *  and may maintain a list of children of the node.
 *  
 *  Maintains a list of elements that are classified by the decision made by this node
 *  If in the middle of construction and this isn't a leaf, elms needs to be divided up by creation
 *  of children
 */
abstract class DTNode[T <: Label#Value, E <: Data[T]](val elms : List[E]) {

  /** Classifies the given data using this node as the root of the tree.*/
  def classify (data : E) : T
  
  /** Returns the node (of this' children) that does the classification for data */
  def classifyingNode( data : E) : DTNode[T, E]
  
  /** Returns a hashmap of children of this node, mapping from child ->
   *  function that takes data and returns true if that data should be mapped
   *  to that child, false otherwise
   */
  def children () : HashMap[DTNode[T, E], AttributeFunction[T, E]]
  
  /** Returns the size of the tree rooted at this */
  def size() : Int
  
  /** Returns the total number of leaves contained in the tree rooted at this */
  def leaves() : Int
  
  /** Returns the max depth of the tree rooted at this. Trees of size 1 have depth 1 */
  def depth() : Int
  
  /** Require that the children override toString */
  def toString() : String
  
  /** Helper for toString implementations that keeps track of how far in depth this is */
  def toStringRec(i : Int) : String
  
  /** Returns a concatination of i tabs */
  protected def tabs(i : Int) : String = {
      var s = ""
      for(i <- 0 until i){
        s = s + "--"
      }
      s
    }
  
  /** Returns the percent success in the tree rooted at this.
   *  Does this by trying to classify each element in elms and checking that against its true classification
   *  returns a double in [0,1] that is the percent predicted correctly
   */
  def labelingAccuracy() : Double = {
    var correctCount = 0.0
    for(e <- elms){
      if(e.label.equals(classify(e))) {
        correctCount = correctCount + 1
      }
    }
    correctCount / elms.size.toDouble
  }

}

/** An ending node in a Decision Tree - internally maintains a label
 *  and classifies all incomming data to that label.
 *  No children - returns an empty hashmap.
 */
class DTLeafNode[T <: Label#Value, E <: Data[T]](val label : T, override val elms : List[E]) 
    extends DTNode[T, E](elms){
  
  val emptyMap = new HashMap[DTNode[T, E], AttributeFunction[T, E]]()
  
  /** Classifies all data according to this' label. */
  override def classify (data : E) = label
  
  /** This classifies all data passed to it */
  override def classifyingNode(data : E) = this
  
  /** Leaf Nodes have no children - returns empty hashmap */
  override def children() = emptyMap
  
  /** Leaf Nodes have size 1 */
  override def size() = 1
  
  /** Leaf Nodes contain 1 leaf */
  override def leaves() = 1
  
  /** Leaf Nodes have depth 1 */
  override def depth() = 1
  
  /** Returns the label as the toString */
  override def toString() = label.toString()
  
  /** Still no recursion to do, just return s */
  override def toStringRec(i : Int) : String = tabs(i) + toString()
  
}

/** A condition to test on - maintains a string representation of itself */
class AttributeFunction[T <: Label#Value, E <: Data[T]](d : E, f : (E => Boolean), s : String){
  /** Constructs from components.
   *  dta - dummy instane to get generic type
   *  i - index of testing in data (0 indexed, as usual)
   *  f - comparison function, with actual value as first input and v as second value
   *  v - the value to enter as the second through nth args
   *  s - a string representation of this function 
   *   */
  def this(dta : E, i : Int, f : ((Double, List[Double]) => Boolean), v : List[Double], s : String) ={
    this(dta, (d : E) => f(d.vals.getOrElse(i, 0.0), v), s)
  }
  /** Creates opposite of given attributefunction */
  def this(a : AttributeFunction[T, E], s : String){
    this(a.aData, a.fun.andThen(a => !a), s)
  }
  /** A Single data reference, to get the generic type */
  private val aData = d
  val fun = f
  
  /** Returns the string representation for the toString */
  override def toString : String = s
}

/** Non-terminal node in a Decision Tree - internally maintains a hashmap
 *  of children to functions.
 *  Classifies an incoming data by iterating through the hashmap and asking each
 *  child if its function returns true for the new data. If so, passes control of
 *  classification off to that child.
 *  
 *  Maintains a naive labeling that represents what this would classify as
 *  were the tree to end here.
 */
class DTAttributeNode[T <: Label#Value, E <: Data[T]](val m : HashMap[DTNode[T, E], AttributeFunction[T, E]], 
    val label : T, override val elms : List[E]) extends DTNode[T, E](elms){
  
  /** Passes control to child for classification. Iterates over all children
   *  and checks if any is matched with a function that accepts data.
   *  Note that if multiple such functions return true, one is chosen arbitrarily.
   *  If no function matches, throws a runtime exception.
   *  Thus in construction, make sure that the functions for each child both
   *  cover all cases and don't overlap.
   */
  override def classify(data : E) : T = {
    for((node, fun) <- m){
      if(fun.fun(data)){
        return node.classify(data)
      }
    }
    //No child to apply data to, either because
    //The data slipped through or there are no children
    // -> classify naively, as if this were a leaf
    naiveClassify(data)
  }
  
  /** Returns the child of this that does the classification for the given data */
  override def classifyingNode(data : E) : DTNode[T, E] = {
    for((node, fun) <- m){
      if(fun.fun(data)){
        return node
      }
    }
    //No child to apply data to, either because
    //The data slipped through or there are no children
    // -> classify naively, as if this were a leaf
    this
  }
  
  /** Classifies Naively, using this as a leaf */
  def naiveClassify(data : E) : T = label
  
  /** Returns the hashmap of children -> acceptance function */
  override def children() : HashMap[DTNode[T, E], AttributeFunction[T, E]]  = m
  
  /** The size of an attributeNodeTree is itself plus its children */
  override def size() = {
    m.foldLeft(1)( (acc : Int, b : (DTNode[T, E], AttributeFunction[T, E])) => acc + b._1.size)
  }
  
  /** Returns the total number of leaves rooted at this */
    /** The size of an attributeNodeTree is itself plus its children */
  override def leaves() = {
    m.foldLeft(0)( (acc : Int, b : (DTNode[T, E], AttributeFunction[T, E])) => acc + b._1.leaves)
  }
  
  /** The depth of an attributeNodeTree is itself plus the max of its children */
  override def depth() : Int = {
    1 + m.foldLeft(0)((acc : Int, b : (DTNode[T, E], AttributeFunction[T, E])) => Math.max(acc,b._1.depth))
  }
  
  /** Recursively shows children for toString */
  override def toString() = {
   toStringRec(0)
  }
  
  /** Recursive toString that maintains tabing along the way */
  def toStringRec(i : Int) : String = {
    var s = ""
    for((node, fun) <- m){
      s = s + tabs(i) + (fun.toString + "\n" + node.toStringRec(i+1) + "\n")
    }
    s.substring(0, s.length() - 1)
  }
  
}