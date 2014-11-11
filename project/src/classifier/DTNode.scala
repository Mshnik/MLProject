package classifier

import scala.collection.mutable.HashMap
import data._
import data.Data._

/** Holder for decision tree algorithms */
object DTNode{
  /** The ID3 DT creation algorithm. Returns the node that represents the root of the tree.
   *  Restricts to the given depth. CurrentDepth should be 1 when first passed in.
   *  splits - (int) different attributes that can be split on, with list[Double] as the splitted values
   *  elms - elements to split
   *  maxDepth - maximum depth to allows a tree to go
   *  currentDepth - current depth of this tree. Increment on successive calls */
  def id3(splits : Map[Int, List[Double]], maxDepth : Int)(
      elms : List[KaggleData], currentDepth : Int) : DTNode = {
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
    var bestCriteria : AttributeFunction = null
    
    
    //For each index, for each splittable value (j in list at index i)
    for(i <- splits.keys; j <- splits(i)){
      //Construct the new criteria for given values of i, j
      val criteria = new AttributeFunction(elms.head, i, (a : Double, b : List[Double]) => a <= b(0), List(j), ("data(" + i + ")<=" + j))
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
      val cOne = id3(splits, maxDepth)(elms.filter(bestCriteria.fun), (currentDepth + 1))
      val cTwo = id3(splits, maxDepth)(elms.filter(bestCriteria.fun.andThen(a => !a)), (currentDepth + 1))
      
      //Create and return this attribute node for the given criteria
      val m = new HashMap[DTNode, AttributeFunction]()
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
  def weightedEntropy(lst : List[Labelable[KaggleLabel.Value]], s : Int) : Double = {
    (lst.length.toDouble / s.toDouble) * entropy(Data.splitByLabel(lst))
  }
  
  /** Returns Log_2(a) */
  def log2(d : Double) : Double = {
    Math.log(d)/Math.log(2)
  }
  
  /** Returns the entropy of the given list partition.
   *  Input is a list of lists - each inner list is a partition of the greater list
   */
  def entropy(mp : Map[KaggleLabel.Value, List[Labelable[KaggleLabel.Value]]]) : Double = {
    val totalSize = mp.foldLeft(0.0)( (a : Double, b : (KaggleLabel.Value,List[Labelable[KaggleLabel.Value]])) => a + b._2.length)
    
    def f (acc : Double, b : (KaggleLabel.Value, List[Labelable[KaggleLabel.Value]])) = {
     val c = if(b._2.length == 0) 0 else
       (b._2.length.toDouble/totalSize)*log2((b._2.length.toDouble/totalSize))
     acc - c
    }
    
    mp.foldLeft(0.0)(f)
  }
}

/** Abstract node class that represents a Decision tree.
 *  May be asked to classify a given data to a label (KaggleLabel.Value),
 *  and may maintain a list of children of the node.
 *  
 *  Maintains a list of elements that are classified by the decision made by this node
 *  If in the middle of construction and this isn't a leaf, elms needs to be divided up by creation
 *  of children
 */
abstract class DTNode(val elms : List[KaggleData]) {

  /** Classifies the given data using this node as the root of the tree.*/
  def classify (data : KaggleData) : KaggleLabel.Value
  
  /** Returns the node (of this' children) that does the classification for data */
  def classifyingNode( data : KaggleData) : DTNode
  
  /** Returns a hashmap of children of this node, mapping from child ->
   *  function that takes data and returns true if that data should be mapped
   *  to that child, false otherwise
   */
  def children () : HashMap[DTNode, AttributeFunction]
  
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
class DTLeafNode(val label : KaggleLabel.Value, override val elms : List[KaggleData]) 
    extends DTNode(elms){
  
  val emptyMap = new HashMap[DTNode, AttributeFunction]()
  
  /** Classifies all data according to this' label. */
  override def classify (data : KaggleData) = label
  
  /** This classifies all data passed to it */
  override def classifyingNode(data : KaggleData) = this
  
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
class AttributeFunction(d : KaggleData, f : (KaggleData => Boolean), s : String){
  /** Constructs from components.
   *  dta - dummy instane to get generic type
   *  i - index of testing in data (0 indexed, as usual)
   *  f - comparison function, with actual value as first input and v as second value
   *  v - the value to enter as the second through nth args
   *  s - a string representation of this function 
   *   */
  def this(dta : KaggleData, i : Int, f : ((Double, List[Double]) => Boolean), v : List[Double], s : String) ={
    this(dta, (d : KaggleData) => f(d.vals.getOrElse(i, 0.0), v), s)
  }
  /** Creates opposite of given attributefunction */
  def this(a : AttributeFunction, s : String){
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
class DTAttributeNode(val m : HashMap[DTNode, AttributeFunction], 
    val label : KaggleLabel.Value, override val elms : List[KaggleData]) extends DTNode(elms){
  
  /** Passes control to child for classification. Iterates over all children
   *  and checks if any is matched with a function that accepts data.
   *  Note that if multiple such functions return true, one is chosen arbitrarily.
   *  If no function matches, throws a runtime exception.
   *  Thus in construction, make sure that the functions for each child both
   *  cover all cases and don't overlap.
   */
  override def classify(data : KaggleData) : KaggleLabel.Value = {
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
  override def classifyingNode(data : KaggleData) : DTNode = {
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
  def naiveClassify(data : KaggleData) : KaggleLabel.Value = label
  
  /** Returns the hashmap of children -> acceptance function */
  override def children() : HashMap[DTNode, AttributeFunction]  = m
  
  /** The size of an attributeNodeTree is itself plus its children */
  override def size() = {
    m.foldLeft(1)( (acc : Int, b : (DTNode, AttributeFunction)) => acc + b._1.size)
  }
  
  /** Returns the total number of leaves rooted at this */
    /** The size of an attributeNodeTree is itself plus its children */
  override def leaves() = {
    m.foldLeft(0)( (acc : Int, b : (DTNode, AttributeFunction)) => acc + b._1.leaves)
  }
  
  /** The depth of an attributeNodeTree is itself plus the max of its children */
  override def depth() : Int = {
    1 + m.foldLeft(0)((acc : Int, b : (DTNode, AttributeFunction)) => Math.max(acc,b._1.depth))
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